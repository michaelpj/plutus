{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- | Testing contracts with HUnit and Tasty
module Language.Plutus.Contract.Test(
      module X
    , TracePredicate
    , Language.Plutus.Contract.Test.not
    , endpointAvailable
    , interestingAddress
    , assertResult
    , assertHooks
    , assertRecord
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , waitingForSlot
    , walletState
    , walletWatchingAddress
    , emulatorLog
    -- * Checking predicates
    , checkPredicate
    ) where

import           Control.Lens                                    (at, folded, to, view, (^.))
import           Control.Monad.Writer                            (MonadWriter (..), Writer, runWriter)
import           Data.Foldable                                   (toList, traverse_)
import           Data.Functor.Contravariant                      (Contravariant (..), Op (..))
import qualified Data.Map                                        as Map
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy(..))
import           Data.Row
import           Data.Semigroup                                  (Min)
import           Data.Sequence                                   (Seq)
import qualified Data.Sequence                                   as Seq
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           GHC.TypeLits                                    (Symbol, KnownSymbol, symbolVal)
import qualified Test.Tasty.HUnit                                as HUnit
import           Test.Tasty.Providers                            (TestTree)

import           Language.Plutus.Contract                        (Contract)
import           Language.Plutus.Contract.Record                 (Record)
import           Language.Plutus.Contract.Resumable              (ResumableError)
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Tx                     (UnbalancedTx)

import qualified Language.Plutus.Contract.Effects.AwaitSlot      as AwaitSlot
import           Language.Plutus.Contract.Effects.ExposeEndpoint (EndpointDescription)
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoints
import qualified Language.Plutus.Contract.Effects.WatchAddress   as WatchAddress
import qualified Language.Plutus.Contract.Effects.WriteTx        as WriteTx

import qualified Ledger.Ada                                      as Ada
import qualified Ledger.AddressMap                               as AM
import           Ledger.Slot                                     (Slot)
import           Ledger.Tx                                       (Address)
import           Ledger.Value                                    (Value)
import qualified Ledger.Value                                    as V
import           Wallet.Emulator                                 (EmulatorAction, EmulatorEvent, Wallet)
import qualified Wallet.Emulator                                 as EM

import Language.Plutus.Contract.Rows.Instances (Event(..), Hooks(..))
import           Language.Plutus.Contract.Trace                  as X

newtype PredF f a = PredF { unPredF :: a -> f Bool }
    deriving Contravariant via (Op (f Bool))

instance Applicative f => Semigroup (PredF f a) where
    l <> r = PredF $ \a -> (&&) <$> unPredF l a <*> unPredF r a

instance Applicative f => Monoid (PredF f a) where
    mappend = (<>)
    mempty = PredF $ const (pure True)

type TracePredicate ρ σ a = PredF (Writer (Seq String)) (InitialDistribution, ContractTraceResult ρ σ a)

hooks
    :: ( Forall σ Monoid
       , Forall σ Semigroup
       , AllUniqueLabels σ
       )
    => Wallet
    -> ContractTraceResult ρ σ a
    -> Hooks σ
hooks w rs =
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in either (const mempty) id (State.execResumable evts con)

record 
    :: forall ρ σ a.
       ( AllUniqueLabels σ
       , Forall σ Semigroup
       , Forall σ Monoid
       )
    => Wallet 
    -> ContractTraceResult ρ σ a
    -> Either ResumableError (Record (Event ρ))
record w rs =
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in fmap (fmap fst . fst) (State.runResumable evts con)

not :: TracePredicate ρ σ a -> TracePredicate ρ σ a
not = PredF . fmap (fmap Prelude.not) . unPredF

checkPredicate
    :: String
    -> Contract ρ σ a
    -> TracePredicate ρ σ a
    -> ContractTrace ρ σ EmulatorAction a ()
    -> TestTree
checkPredicate nm con predicate action =
    HUnit.testCaseSteps nm $ \step ->
        case runTrace con action of
            (Left err, _) ->
                HUnit.assertFailure $ "EmulatorAction failed. " ++ show err
            (Right (_, st), ms) -> do
                let dt = ContractTraceResult ms st
                    (result, emLog) = runWriter $ unPredF predicate (defaultDist, dt)
                if result then pure () else traverse_ step emLog
                HUnit.assertBool nm result

endpointAvailable
    :: forall (s :: Symbol) ρ σ a.
       ( HasType s (Set EndpointDescription) σ
       , KnownSymbol s
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ Semigroup
       )
    => Wallet
    -> TracePredicate ρ σ a
endpointAvailable w = PredF $ \(_, r) -> do
    if Endpoints.isActive @s (hooks w r)
    then pure True
    else do
        tellSeq ["missing endpoint:" ++ symbolVal (Proxy :: Proxy s)]
        pure False

interestingAddress
    :: forall ρ σ a.
       ( HasType "interesting addresses" (Set Address) σ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ Semigroup
       )
    => Wallet
    -> Address
    -> TracePredicate ρ σ a
interestingAddress w addr = PredF $ \(_, r) -> do
    let hks = WatchAddress.addresses (hooks w r)
    if addr `Set.member` hks
    then pure True
    else do
        tellSeq ["Interesting addresses:", unlines (show <$> toList hks), "missing address:", show addr]
        pure False

tx
    :: forall ρ σ a.
       ( HasType "tx" [UnbalancedTx] σ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ Semigroup)
    => Wallet
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate ρ σ a
tx w flt nm = PredF $ \(_, r) -> do
    let hks = WriteTx.transactions (hooks w r)
    if any flt hks
    then pure True
    else do
        tellSeq ["Unbalanced transactions;", unlines (fmap show hks), "No transaction with '" <> nm <> "'"]
        pure False

walletState 
    :: forall ρ σ a.
       Wallet 
    -> (EM.WalletState -> Bool) 
    -> String 
    -> TracePredicate ρ σ a
walletState w flt nm = PredF $ \(_, r) -> do
    let ws = view (at w) $ EM._walletStates $  _ctrEmulatorState r
    case ws of
        Nothing -> do
            tellSeq ["Wallet state of '" <> show w <> "' not found"]
            pure False
        Just st ->
            if flt st
            then pure True
            else do
                tellSeq ["Wallet state of " <> show w <> ":", show st, "Fails '" <> nm <> "'"]
                pure False

walletWatchingAddress 
    :: forall ρ σ a.
       Wallet
    -> Address
    -> TracePredicate ρ σ a
walletWatchingAddress w addr =
    let desc = "watching address " <> show addr in
    walletState w (Map.member addr . AM.values . view EM.addressMap) desc

assertEvents 
    :: forall ρ σ a.
       (Forall ρ Show)
    => Wallet
    -> ([Event ρ] -> Bool)
    -> String
    -> TracePredicate ρ σ a
assertEvents w pr nm = PredF $ \(_, r) -> do
    let es = fmap toList (view (ctsEvents . at w) $ _ctrTraceState r)
    case es of
        Nothing -> do
            tellSeq ["Event log for '" <> show w <> "' not found"]
            pure False
        Just lg ->
            if pr lg
            then pure True
            else do
                tellSeq ["Event log for '" <> show w <> ":", unlines (fmap show lg), "Fails '" <> nm <> "'"]
                pure False

waitingForSlot
    :: forall ρ σ a.
       ( HasType "slot" (Maybe (Min Slot)) σ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ Semigroup
       )
    => Wallet
    -> Slot
    -> TracePredicate ρ σ a
waitingForSlot w sl = PredF $ \(_, r) ->
    case AwaitSlot.nextSlot (hooks w r) of
        Nothing -> do
            tellSeq [show w <> " not waiting for any slot notifications. Expected: " <>  show sl]
            pure False
        Just sl' ->
            if sl == sl'
            then pure True
            else do
                tellSeq [show w <> " waiting for " <> show sl', "Expected: " <> show sl]
                pure False

emulatorLog
    :: forall ρ σ a. 
       ()
    => ([EmulatorEvent] -> Bool)
    -> String
    -> TracePredicate ρ σ a
emulatorLog f nm = PredF $ \(_, r) ->
    let lg = EM._emulatorLog $ _ctrEmulatorState r in
    if f lg
    then pure True
    else do
        tellSeq ["Emulator log:", unlines (fmap show lg), "Fails '" <> nm <> "'"]
        pure False

anyTx
    :: forall ρ σ a. 
       ( HasType "tx" [UnbalancedTx] σ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ Semigroup
       )
    => Wallet
    -> TracePredicate ρ σ a
anyTx w = tx w (const True) "anyTx"

assertHooks 
    :: forall ρ σ a. 
       ( AllUniqueLabels σ 
       , Forall σ Monoid
       , Forall σ Semigroup
       , Forall σ Show
       )
    => Wallet
    -> (Hooks σ -> Bool)
    -> String
    -> TracePredicate ρ σ a
assertHooks w p nm = PredF $ \(_, rs) ->
    let hks = hooks w rs in
    if p hks
    then pure True
    else do
        tellSeq ["Hooks:", show hks, "Failed '" <> nm <> "'"]
        pure False

assertRecord 
    :: forall ρ σ a. 
       ( Forall ρ Show
       , Forall σ Semigroup
       , Forall σ Monoid
       , AllUniqueLabels σ
       )
    => Wallet 
    -> (Record (Event ρ) -> Bool)
    -> String
    -> TracePredicate ρ σ a
assertRecord w p nm = PredF $ \(_, rs) ->
    case record w rs of
        Right r
            | p r -> pure True
            | otherwise -> do
                tellSeq ["Record: ", show r, "Failed '" <> nm <> "'"]
                pure False
        Left err -> do
            tellSeq ["Record failed with", show err, "in '" <> nm <> "'"]
            pure False

assertResult
    :: forall ρ σ a. 
       ( Forall ρ Show
       , AllUniqueLabels σ
       , Forall σ Semigroup
       , Forall σ Monoid
       )
    => Wallet
    -> (Maybe a -> Bool)
    -> String
    -> TracePredicate ρ σ a
assertResult w p nm = PredF $ \(_, rs) ->
    let evts = rs ^. ctrTraceState . ctsEvents . at w . folded . to toList
        con  = rs ^. ctrTraceState . ctsContract
        result = State.runResumable evts con
    in
        case fmap fst result of
            Left err
                | p Nothing -> pure True
                | otherwise -> do
                    tellSeq ["Resumable error", show err, "in '" <> nm <> "'"]
                    pure False
            Right (Left openRec)
                | p Nothing -> pure True
                | otherwise -> do
                    tellSeq ["Open record", show openRec, "in '" <> nm <> "'"]
                    pure False
            Right (Right (closedRec, a))
                | p (Just a) -> pure True
                | otherwise -> do
                    tellSeq ["Closed record", show closedRec, "failed with '" <> nm <> "'"]
                    pure False

walletFundsChange 
    :: forall ρ σ a. 
       ()
    => Wallet
    -> Value
    -> TracePredicate ρ σ a
walletFundsChange w dlt = PredF $ \(initialDist, ContractTraceResult{_ctrEmulatorState = st}) ->
        let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in if initialValue `V.plus` dlt == finalValue
        then pure True
        else do
            tellSeq ["Expected funds to change by", show dlt, "but they changed by", show (finalValue `V.minus` initialValue)]
            pure False

tellSeq :: MonadWriter (Seq a) m => [a] -> m ()
tellSeq = tell . Seq.fromList
