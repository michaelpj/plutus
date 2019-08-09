{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Big hammer, but helps
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-name-shadowing #-}

{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

{-| = Marlowe: financial contracts on Cardano Computation Layer

Here we present a reference implementation of Marlowe, domain-specific language targeted at
the execution of financial contracts in the style of Peyton Jones et al
on Cardano Computation Layer.

The implementation is based on semantics described in paper
<https://iohk.io/research/papers/#2WHKDRA8 'Marlowe: financial contracts on blockchain'>
by Simon Thompson and Pablo Lamela Seijas

== Semantics

Semantics is based on <https://github.com/input-output-hk/marlowe/blob/stable/src/Semantics.hs>

Marlowe Contract execution is a chain of transactions,
where remaining contract and its state is passed through /Data Script/,
and actions (i.e. /Choices/ and /Oracle Values/) are passed as
/Redeemer Script/

/Validation Script/ is always the same Marlowe interpreter implementation, available below.

Both /Redeemer Script/ and /Data Script/ have the same structure:
@(Input, MarloweData)@

where

* 'Input' contains contract actions (i.e. /Pay/, /Redeem/), /Choices/ and /Oracle Values/,
* 'MarloweData' contains remaining 'Contract' and its 'State'
* 'State' is a set of 'Commit's plus a set of made 'Choice's

To spend 'TxOut' secured by Marlowe /Validator Script/, a user must provide /Redeemer Script/
that is a tuple of an 'Input' and expected output of Marlowe 'Contract' interpretation for
the given 'Input', i.e. 'Contract' and 'State'.

To ensure that user provides valid remainig 'Contract' and 'State'
Marlowe /Validator Script/ compares evaluated contract and state with provided by user,
and rejects a transaction if those don't match.

To ensure that remaining contract's /Data Script/ has the same 'Contract' and 'State'
as was passed with /Redeemer Script/, we check that /Data Script/ hash is
the same as /Redeemer Script/.
That's why those are of the same structure @(Input, MarloweData)@.

== Example

Consider simple payment contract, where Alice commits to pay 100 Ada to Bob before timeout1.
She can get back committed money if Bob didn't ask for payment before timeout2.

> let Alice = PubKey 1
> let Bob   = PubKey 2
> let timeout1 = 23
> let timeout2 = 50
> let contract = CommitCash (IdentCC 1) Alice (Value 100) timeout1 timeout2
>             (Pay (IdentPay 1) Alice Bob (Committed (IdentCC 1)) timeout2 Null)
>             Null

Alice commits:

> let input = Input (Commit (IdentCC 1) (txHash `signedBy` Alice)) [] []

Bob demands payment:

> let input = Input (Payment (IdentPay 1) (txHash `signedBy` Bob)) [] []

Or, in case Bob didn't demand payment before timeout2, Alice can require a redeem of her commit:

> let input = Input (Redeem (IdentCC 1) (txHash `signedBy` Alice)) [] []
-}

module Language.Marlowe3.Common where

import qualified Prelude                    as Haskell

import           GHC.Generics               (Generic)
import           Language.Marlowe.Pretty    (Pretty, prettyFragment)
import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Lift     (makeLift)
import qualified Language.PlutusTx.AssocMap as Map
import           Language.PlutusTx.AssocMap (Map)
import           Language.PlutusTx.Prelude
import           Ledger                     (PubKey (..), Signature (..), Slot (..))
import           Ledger.Ada                 (Ada)
import qualified Ledger.Ada                 as Ada
import           Ledger.Interval            (Interval (..))
import           Ledger.Slot                    (SlotRange)
import           Ledger.Scripts             (DataScriptHash (..), RedeemerHash (..))
import           Ledger.Validation
import           LedgerBytes                (LedgerBytes (..))

{-# ANN module ("HLint: ignore"::String) #-}

type Party = PubKey
type Timeout = Slot
type Money = Ada
type ChosenNum = Integer
type Bound = (Integer, Integer)

data AccountId = AccountId Integer Party deriving (Show)

data ChoiceId = ChoiceId Integer Party deriving (Show)

data ValueId  = ValueId Integer deriving (Show)

data Value = AvailableMoney AccountId
           | Constant Integer
           | NegValue Value
           | AddValue Value Value
           | SubValue Value Value
           | ChoiceValue ChoiceId Value
           | SlotIntervalStart
           | SlotIntervalEnd
           | UseValue ValueId
  deriving (Show)

data Observation = AndObs Observation Observation
                 | OrObs Observation Observation
                 | NotObs Observation
                 | ChoseSomething ChoiceId
                 | ValueGE Value Value
                 | ValueGT Value Value
                 | ValueLT Value Value
                 | ValueLE Value Value
                 | ValueEQ Value Value
                 | TrueObs
                 | FalseObs
  deriving (Show)


data Action = Deposit AccountId Party Value
            | Choice ChoiceId [Bound]
            | Notify Observation
  deriving (Show)

data Payee = Account AccountId
           | Party Party
  deriving (Show)

data Case = Case Action Contract
  deriving (Show)

data Contract = Refund
              | Pay AccountId Payee Value Contract
              | If Observation Contract Contract
              | When [Case] Timeout Contract
              | Let ValueId Value Contract
  deriving (Show)

data State = State { accounts :: Map AccountId Ada
                   , choices  :: Map ChoiceId ChosenNum
                   , boundValues :: Map ValueId Integer
                   , minSlot :: Slot }
  deriving (Show)

data Environment = Environment { slotRange :: SlotRange }
  deriving (Show)

data Input = IDeposit AccountId Party Money
           | IChoice ChoiceId ChosenNum
           | INotify
  deriving (Show)

data ReduceWarning = ReduceNoWarning
                   | ReduceNonPositivePay AccountId Payee Integer
                   | ReducePartialPay AccountId Payee Integer Integer
                                    -- ^ src    ^ dest ^ paid ^ expected
                   | ReduceShadowing ValueId Integer Integer
                                      -- oldVal ^  newVal ^
                   deriving (Show)

data ReduceEffect = ReduceNoEffect
                  | ReduceNormalPay Party Money
                  deriving (Show)

data ReduceError = ReduceAmbiguousSlotInterval deriving (Show)

data ReduceResult = Reduced ReduceWarning ReduceEffect State Contract
                  | NotReduced
                  | ReduceError ReduceError
                  deriving (Show)

{-|
    This data type is a content of a contract's /Data Script/
-}
data MarloweData = MarloweData {
        marloweState    :: State,
        marloweContract :: Contract
    } deriving (Show)

instance Eq AccountId where
    {-# INLINABLE (==) #-}
    (AccountId n1 p1) == (AccountId n2 p2) = n1 == n2 && p1 == p2

instance Eq ChoiceId where
    {-# INLINABLE (==) #-}
    (ChoiceId n1 p1) == (ChoiceId n2 p2) = n1 == n2 && p1 == p2

instance Eq ValueId where
    {-# INLINABLE (==) #-}
    (ValueId n1) == (ValueId n2) = n1 == n2

instance Eq Payee where
    {-# INLINABLE (==) #-}
    Account acc1 == Account acc2 = acc1 == acc2
    Party p1 == Party p2 = p1 == p2
    _ == _ = False

instance Eq ReduceWarning where
    {-# INLINABLE (==) #-}
    ReduceNoWarning == ReduceNoWarning = True
    (ReduceNonPositivePay acc1 p1 a1) == (ReduceNonPositivePay acc2 p2 a2) = acc1 == acc2 && p1 == p2 && a1 == a2
    (ReducePartialPay acc1 p1 a1 e1) == (ReducePartialPay acc2 p2 a2 e2) = acc1 == acc2 && p1 == p2 && a1 == a2 && e1 == e2
    (ReduceShadowing v1 old1 new1) == (ReduceShadowing v2 old2 new2) = v1 == v2 && old1 == old2 && new1 == new2
    _ == _ = False

instance Eq ReduceEffect where
    {-# INLINABLE (==) #-}
    ReduceNoEffect == ReduceNoEffect = True
    ReduceNormalPay p1 m1 == ReduceNormalPay p2 m2 = p1 == p2 && m1 == m2
    _ == _ = False


makeLift ''AccountId
makeLift ''ChoiceId
makeLift ''ValueId
makeLift ''Value
makeLift ''Observation
makeLift ''Action
makeLift ''Payee
makeLift ''Case
makeLift ''Contract
makeLift ''State
makeLift ''Environment
makeLift ''Input
makeLift ''MarloweData
makeLift ''ReduceWarning
makeLift ''ReduceEffect


{-# INLINABLE accountOwner #-}
accountOwner :: AccountId -> Party
accountOwner (AccountId _ party) = party


{-# INLINABLE inBounds #-}
inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = any (\(l, u) -> num >= l && num <= u)


{-# INLINABLE evaluateValue #-}
{-|
    Evaluates @Value@ given current @State@ and @Environment@
-}
evaluateValue :: Environment -> State -> Value -> Integer
evaluateValue Environment{..} State{..} value = let
    evalValue :: Value -> Integer
    evalValue value = case value of
        AvailableMoney accountId -> case Map.lookup accountId accounts of
            Just x  -> Ada.toInt x
            Nothing -> 0
        Constant v -> v
        NegValue v -> (-1) * evalValue v
        AddValue lhs rhs -> evalValue lhs `Builtins.addInteger` evalValue rhs
        SubValue lhs rhs -> evalValue lhs `Builtins.multiplyInteger` evalValue rhs
        ChoiceValue choiceId def -> case Map.lookup choiceId choices of
            Just x  -> x
            Nothing -> evalValue def
        SlotIntervalStart -> maybe 0 getSlot (ivFrom slotRange) -- TODO fixme
        SlotIntervalEnd -> maybe 0 getSlot (ivTo slotRange) -- TODO fixme
        UseValue valueId -> case Map.lookup valueId boundValues of
          Just x  -> x
          Nothing -> 0
    in evalValue value


-- | Evaluate 'Observation' to 'Bool'.
{-# INLINABLE evaluateObservation #-}
evaluateObservation :: Environment -> State -> Observation -> Bool
evaluateObservation env (state@State{..}) obs = go obs
  where
    evalValue = evaluateValue env state
    go :: Observation -> Bool
    go obs = case obs of
        AndObs obs1 obs2 -> go obs1 && go obs2
        OrObs obs1 obs2 -> go obs1 || go obs2
        NotObs obs -> not (go obs)
        ChoseSomething choiceId -> isJust (Map.lookup choiceId choices)
        ValueGE a b -> evalValue a >= evalValue b
        ValueGT a b -> evalValue a >  evalValue b
        ValueLT a b -> evalValue a <  evalValue b
        ValueLE a b -> evalValue a <= evalValue b
        ValueEQ a b -> evalValue a == evalValue b
        TrueObs -> True
        FalseObs -> False


-- Pick the first account with money in it
{-# INLINABLE refundOne #-}
refundOne :: Map AccountId Money -> Maybe ((Party, Money), Map AccountId Money)
refundOne accounts = case Map.toList accounts of
    [] -> Nothing
    (accId, balance) : rest -> if balance > 0
                           then Just ((accountOwner accId, balance), Map.fromList rest)
                           else refundOne (Map.fromList rest)

-- Obtains the amount of money available an account
{-# INLINABLE moneyInAccount #-}
moneyInAccount :: AccountId -> Map AccountId Money -> Money
moneyInAccount accId accounts = maybe 0 id $ Map.lookup accId accounts

-- Add the given amount of money to an accoun (only if it is positive)
-- Return the updated Map
{-# INLINABLE addMoneyToAccount #-}
addMoneyToAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
addMoneyToAccount accId money accounts | money <= 0  = accounts
                                       | otherwise = updateMoneyInAccount accId newAvMoney accounts
  where
    avMoney = moneyInAccount accId accounts
    newAvMoney = avMoney + money

-- Sets the amount of money available in an account
{-# INLINABLE updateMoneyInAccount #-}
updateMoneyInAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
updateMoneyInAccount accId money | money <= 0 = Map.delete accId
                                 | otherwise = Map.insert accId money

{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
{-# INLINABLE giveMoney #-}
giveMoney :: Payee -> Money -> Map AccountId Money -> (ReduceEffect, Map AccountId Money)
giveMoney (Party   party) money accounts = (ReduceNormalPay party money, accounts)
giveMoney (Account accId) money accounts = (ReduceNoEffect, newAccs)
  where newAccs = addMoneyToAccount accId money accounts

-- Withdraw up to the given amount of money from an account
-- Return the amount of money withdrawn
{-# INLINABLE withdrawMoneyFromAccount #-}
withdrawMoneyFromAccount
  :: AccountId -> Money -> Map AccountId Money -> (Money, Map AccountId Money)
withdrawMoneyFromAccount accId money accounts = (withdrawnMoney, newAcc)
 where
  avMoney        = moneyInAccount accId accounts
  withdrawnMoney = min avMoney money
  newAvMoney     = avMoney - withdrawnMoney
  newAcc         = updateMoneyInAccount accId newAvMoney accounts


-- | Carry a step of the contract with no inputs
{-# INLINABLE reduce #-}
reduce :: Environment -> State -> Contract -> ReduceResult
reduce env state@State{..} contract = case contract of
    Refund -> case refundOne accounts of
        Just ((party, money), newAccounts) ->
            let newState = state { accounts = newAccounts }
            in  Reduced ReduceNoWarning (ReduceNormalPay party money) newState Refund
        Nothing -> NotReduced
    Pay accId payee val cont -> if amountToPay <= 0
        then Reduced (ReduceNonPositivePay accId payee amountToPay) ReduceNoEffect state cont
        else let
                (paidMoney, newAccs) = withdrawMoneyFromAccount accId (Ada.fromInt amountToPay) accounts
                paidAmount = Ada.toInt paidMoney
                warning = if paidAmount < amountToPay
                          then ReducePartialPay accId payee paidAmount amountToPay
                          else ReduceNoWarning
                (payEffect, finalAccs) = giveMoney payee paidMoney newAccs
            in Reduced warning payEffect (state { accounts = finalAccs }) cont
      where amountToPay = evaluateValue env state val
    If obs cont1 cont2 -> Reduced ReduceNoWarning ReduceNoEffect state cont
      where cont = if evaluateObservation env state obs then cont1 else cont2
    When _ timeout cont
        | endSlot < timeout -> NotReduced
        | startSlot >= timeout -> Reduced ReduceNoWarning ReduceNoEffect state cont
        | otherwise -> ReduceError ReduceAmbiguousSlotInterval
      where
        startSlot = maybe 0 id (ivFrom $ slotRange env)
        endSlot = maybe 0 id (ivTo $ slotRange env)
    Let valId val cont -> Reduced warn ReduceNoEffect newState cont
      where
        evVal = evaluateValue env state val
        newState = state { boundValues = Map.insert valId evVal boundValues }
        warn = case Map.lookup valId boundValues of
                  Just oldVal -> ReduceShadowing valId oldVal evVal
                  Nothing -> ReduceNoWarning


data ReduceAllResult = ReducedAll [ReduceWarning] [ReduceEffect] State Contract
                     | ReduceAllError ReduceError
                     deriving (Show)


reduceAll :: Environment -> State -> Contract -> ReduceAllResult
reduceAll env state contract = reduceAllAux env state contract [] []
  where
    -- Reduce until it cannot be reduced more
    reduceAllAux :: Environment -> State -> Contract -> [ReduceWarning] -> [ReduceEffect] -> ReduceAllResult
    reduceAllAux env state contract warnings effects = case reduce env state contract of
        Reduced warning effect newState cont -> let
            newWarnings = if warning == ReduceNoWarning then warnings else warning : warnings
            newEffects  = if effect  == ReduceNoEffect  then effects  else effect : effects
            in  reduceAllAux env newState cont newWarnings newEffects
        ReduceError err -> ReduceAllError err
        NotReduced -> ReducedAll (reverse warnings) (reverse effects) state contract
