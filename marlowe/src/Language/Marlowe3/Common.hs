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
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
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

import qualified Language.PlutusTx.Builtins as Builtins
import           Language.PlutusTx.Lift     (makeLift)
import qualified Language.PlutusTx.AssocMap as Map
import           Language.PlutusTx.AssocMap (Map)
import           Language.PlutusTx.Prelude
import           Ledger                     ( PubKey(..)
                                            , Signature(..)
                                            , Slot(..)
                                            , applyScript
                                            , compileScript
                                            , lifted
                                            )
import           Ledger.Ada                 (Ada)
import qualified Ledger.Ada                 as Ada
import           Ledger.Interval            (Interval (..))
import           Ledger.Scripts             ( HashedDataScript(..)
                                            , ValidatorHash(..)
                                            , ValidatorScript(..)
                                            , DataScriptHash(..)
                                            )
import           Ledger.Validation
import           LedgerBytes                (LedgerBytes (..))

{-# ANN module ("HLint: ignore"::String) #-}

type Party = PubKey
-- type ChoiceName = ByteString
type NumAccount = Integer
type Timeout = Slot
type Money = Ada
type ChosenNum = Integer
type Bound = (Integer, Integer)
type SlotInterval = (Slot, Slot)

eqPubKey :: Party -> Party -> Bool
eqPubKey (PubKey (LedgerBytes pk1)) (PubKey (LedgerBytes pk2)) = pk1 == pk2

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

{-| Plutus doesn't support mutually recursive data types yet.
    datatype Case is mutually recurvive with @Contract@
-}
type Case = (Action, Contract)

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

data Environment = Environment { slotInterval :: SlotInterval }
  deriving (Show)

data Input = IDeposit AccountId Party Money
           | IChoice ChoiceId ChosenNum
           | INotify
  deriving (Show)

data IntervalError = InvalidInterval SlotInterval
                   | IntervalInPastError Slot SlotInterval
  deriving (Show)

data IntervalResult = IntervalTrimmed Environment State
                    | IntervalError IntervalError
  deriving (Show)

data Payment = Payment Party Money
  deriving (Show)

data ReduceEffect = ReduceWithPayment Payment
                  | ReduceNoPayment
  deriving (Show)

data ReduceWarning = ReduceNoWarning
                   | ReduceNonPositivePay AccountId Payee Integer
                   | ReducePartialPay AccountId Payee Money Money
                                   -- ^ src    ^ dest ^ paid ^ expected
                   | ReduceShadowing ValueId Integer Integer
                                    -- oldVal ^  newVal ^
  deriving (Show)


data ReduceStepResult = Reduced ReduceWarning ReduceEffect State Contract
                      | NotReduced
                      | AmbiguousSlotIntervalReductionError
  deriving (Show)

data ReduceResult = ContractQuiescent [ReduceWarning] [Payment] State Contract
                  | RRAmbiguousSlotIntervalError
  deriving (Show)

data ApplyResult = Applied State Contract
                 | ApplyNoMatchError
  deriving (Show)

data ApplyAllResult = ApplyAllSuccess [ReduceWarning] [Payment] State Contract
                    | ApplyAllNoMatchError
                    | ApplyAllAmbiguousSlotIntervalError
  deriving (Show)

data TransactionError = TEAmbiguousSlotIntervalError
                      | TEApplyNoMatchError
                      | TEIntervalError IntervalError
                      | TEUselessTransaction
  deriving (Show)

data TransactionInput = TransactionInput
    { txInterval :: SlotInterval
    , txInputs   :: [Input] }
  deriving (Show)


data TransactionOutput =
    TransactionOutput
        { txOutWarnings :: [ReduceWarning]
        , txOutPayments :: [Payment]
        , txOutState    :: State
        , txOutContract :: Contract }
    | Error TransactionError
  deriving (Show)

{-|
    This data type is a content of a contract's /Data Script/
-}
data MarloweData = MarloweData {
        marloweCreator  :: Party,
        marloweState    :: State,
        marloweContract :: Contract
    } deriving (Show)

instance Eq AccountId where
    {-# INLINABLE (==) #-}
    (AccountId n1 p1) == (AccountId n2 p2) = n1 == n2 && p1 `eqPubKey` p2

instance Eq ChoiceId where
    {-# INLINABLE (==) #-}
    (ChoiceId n1 p1) == (ChoiceId n2 p2) = n1 == n2 && p1 `eqPubKey` p2

instance Eq ValueId where
    {-# INLINABLE (==) #-}
    (ValueId n1) == (ValueId n2) = n1 == n2

instance Eq Payee where
    {-# INLINABLE (==) #-}
    Account acc1 == Account acc2 = acc1 == acc2
    Party p1 == Party p2 = p1 `eqPubKey` p2
    _ == _ = False

instance Eq Payment where
    {-# INLINABLE (==) #-}
    Payment p1 m1 == Payment p2 m2 = p1 `eqPubKey` p2 && m1 == m2

instance Eq ReduceWarning where
    {-# INLINABLE (==) #-}
    ReduceNoWarning == ReduceNoWarning = True
    (ReduceNonPositivePay acc1 p1 a1) == (ReduceNonPositivePay acc2 p2 a2) =
        acc1 == acc2 && p1 == p2 && a1 == a2
    (ReducePartialPay acc1 p1 a1 e1) == (ReducePartialPay acc2 p2 a2 e2) =
        acc1 == acc2 && p1 == p2 && a1 == a2 && e1 == e2
    (ReduceShadowing v1 old1 new1) == (ReduceShadowing v2 old2 new2) =
        v1 == v2 && old1 == old2 && new1 == new2
    _ == _ = False

instance Eq ReduceEffect where
    {-# INLINABLE (==) #-}
    ReduceNoPayment == ReduceNoPayment = True
    ReduceWithPayment p1 == ReduceWithPayment p2 = p1 == p2
    _ == _ = False


instance Eq Value where
    {-# INLINABLE (==) #-}
    AvailableMoney acc1 == AvailableMoney acc2 = acc1 == acc2
    Constant i1 == Constant i2 = i1 == i2
    NegValue val1 == NegValue val2 = val1 == val2
    AddValue val1 val2 == AddValue val3 val4 = val1 == val3 && val2 == val4
    SubValue val1 val2 == SubValue val3 val4 = val1 == val3 && val2 == val4
    ChoiceValue cid1 val1 == ChoiceValue cid2 val2 = cid1 == cid2 && val1 == val2
    SlotIntervalStart == SlotIntervalStart = True
    SlotIntervalEnd   == SlotIntervalEnd   = True
    UseValue val1 == UseValue val2 = val1 == val2
    _ == _ = False

instance Eq Observation where
    {-# INLINABLE (==) #-}
    AndObs o1l o2l == AndObs o1r o2r = o1l == o1r && o2l == o2r
    OrObs  o1l o2l == OrObs  o1r o2r = o1l == o1r && o2l == o2r
    NotObs ol == NotObs or = ol == or
    ChoseSomething cid1 == ChoseSomething cid2 = cid1 == cid2
    ValueGE v1l v2l == ValueGE v1r v2r = v1l == v1r && v2l == v2r
    ValueGT v1l v2l == ValueGT v1r v2r = v1l == v1r && v2l == v2r
    ValueLT v1l v2l == ValueLT v1r v2r = v1l == v1r && v2l == v2r
    ValueLE v1l v2l == ValueLE v1r v2r = v1l == v1r && v2l == v2r
    ValueEQ v1l v2l == ValueEQ v1r v2r = v1l == v1r && v2l == v2r
    TrueObs  == TrueObs  = True
    FalseObs == FalseObs = True
    _ == _ = False


instance Eq Action where
    {-# INLINABLE (==) #-}
    Deposit acc1 party1 val1 == Deposit acc2 party2 val2 =
        acc1 == acc2 && party1 `eqPubKey` party2 && val1 == val2
    Choice cid1 bounds1 == Choice cid2 bounds2 =
        cid1 == cid2 && let
            bounds = zip bounds1 bounds2
            checkBound ((low1, high1), (low2, high2)) = low1 == low2 && high1 == high2
            in all checkBound bounds
    Notify obs1 == Notify obs2 = obs1 == obs2
    _ == _ = False

instance Eq Contract where
    {-# INLINABLE (==) #-}
    Refund == Refund = True
    Pay acc1 payee1 value1 cont1 == Pay acc2 payee2 value2 cont2 =
        acc1 == acc2 && payee1 == payee2 && value1 == value2 && cont1 == cont2
    If obs1 cont1 cont2 == If obs2 cont3 cont4 =
        obs1 == obs2 && cont1 == cont3 && cont2 == cont4
    When cases1 timeout1 cont1 == When cases2 timeout2 cont2 =
        timeout1 == timeout2 && cont1 == cont2
        && let cases = (zip cases1 cases2)
               checkCase ((action1, cont1), (action2, cont2)) = action1 == action2 && cont1 == cont2
           in all checkCase cases
    Let valId1 val1 cont1 == Let valId2 val2 cont2 =
        valId1 == valId2 && val1 == val2 && cont1 == cont2
    _ == _ = False


instance Eq State where
    {-# INLINABLE (==) #-}
    l == r = minSlot l == minSlot r
        && accounts l == accounts r
        && choices l == choices r
        && boundValues l == boundValues r



makeLift ''AccountId
makeLift ''ChoiceId
makeLift ''ValueId
makeLift ''Value
makeLift ''Observation
makeLift ''Action
makeLift ''Payee
makeLift ''Contract
makeLift ''State
makeLift ''Environment
makeLift ''Input
makeLift ''IntervalError
makeLift ''IntervalResult
makeLift ''Payment
makeLift ''ReduceEffect
makeLift ''ReduceWarning
makeLift ''ReduceStepResult
makeLift ''ReduceResult
makeLift ''ApplyResult
makeLift ''ApplyAllResult
makeLift ''TransactionError
makeLift ''TransactionOutput
makeLift ''MarloweData


{-# INLINABLE accountOwner #-}
accountOwner :: AccountId -> Party
accountOwner (AccountId _ party) = party


{-# INLINABLE inBounds #-}
inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = any (\(l, u) -> num >= l && num <= u)


{-# INLINABLE fixInterval #-}
fixInterval :: SlotInterval -> State -> IntervalResult
fixInterval interval state =
    case interval of
        (low, high)
          | high < low -> IntervalError (InvalidInterval interval)
          | otherwise -> let
            curMinSlot = minSlot state
            -- newLow is both new "low" and new "minSlot" (the lower bound for slotNum)
            newLow = max low curMinSlot
            -- We know high is greater or equal than newLow (prove)
            curInterval = (newLow, high)
            env = Environment { slotInterval = curInterval }
            newState = state { minSlot = newLow }
            in if high < curMinSlot then IntervalError (IntervalInPastError curMinSlot interval)
            else IntervalTrimmed env newState


{-|
  Evaluates @Value@ given current @State@ and @Environment@
-}
{-# INLINABLE evalValue #-}
evalValue :: Environment -> State -> Value -> Integer
evalValue env state value = let
    eval = evalValue env state
    in case value of
        AvailableMoney accId ->
            case Map.lookup accId (accounts state) of
                Just x  -> Ada.getLovelace x
                Nothing -> 0
        Constant integer     -> integer
        NegValue val         -> (-1) * eval val
        AddValue lhs rhs     -> eval lhs + eval rhs
        SubValue lhs rhs     -> eval lhs - eval rhs
        ChoiceValue choiceId defVal ->
            case Map.lookup choiceId (choices state) of
                Just x  -> x
                Nothing -> eval defVal
        SlotIntervalStart    -> getSlot (fst (slotInterval env))
        SlotIntervalEnd      -> getSlot (snd (slotInterval env))
        UseValue valId       ->
            case Map.lookup valId (boundValues state) of
                Just x  -> x
                Nothing -> 0


-- | Evaluate 'Observation' to 'Bool'.
{-# INLINABLE evalObservation #-}
evalObservation :: Environment -> State -> Observation -> Bool
evalObservation env state obs = let
    evalObs = evalObservation env state
    evalVal = evalValue env state
    in case obs of
        AndObs lhs rhs       -> evalObs lhs && evalObs rhs
        OrObs lhs rhs        -> evalObs lhs || evalObs rhs
        NotObs subObs        -> not (evalObs subObs)
        ChoseSomething choiceId -> isJust (Map.lookup choiceId (choices state))
        ValueGE lhs rhs      -> evalVal lhs >= evalVal rhs
        ValueGT lhs rhs      -> evalVal lhs > evalVal rhs
        ValueLT lhs rhs      -> evalVal lhs < evalVal rhs
        ValueLE lhs rhs      -> evalVal lhs <= evalVal rhs
        ValueEQ lhs rhs      -> evalVal lhs == evalVal rhs
        TrueObs              -> True
        FalseObs             -> False


-- Pick the first account with money in it
{-# INLINABLE refundOne #-}
refundOne :: Map AccountId Money -> Maybe ((Party, Money), Map AccountId Money)
refundOne accounts = case Map.toList accounts of
    -- FIXME There should be an ascending order of accounts. Use AVL Tree for Maps
    [] -> Nothing
    (accId, balance) : rest ->
        if balance > 0
        then Just ((accountOwner accId, balance), Map.fromList rest)
        else refundOne (Map.fromList rest)

-- Obtains the amount of money available an account
{-# INLINABLE moneyInAccount #-}
moneyInAccount :: AccountId -> Map AccountId Money -> Money
moneyInAccount accId accounts = case Map.lookup accId accounts of
    Just x -> x
    Nothing -> Ada.lovelaceOf 0


-- Sets the amount of money available in an account
{-# INLINABLE updateMoneyInAccount #-}
updateMoneyInAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
updateMoneyInAccount accId money =
    if Ada.getLovelace money <= 0 then Map.delete accId else Map.insert accId money


-- Add the given amount of money to an accoun (only if it is positive)
-- Return the updated Map
{-# INLINABLE addMoneyToAccount #-}
addMoneyToAccount :: AccountId -> Money -> Map AccountId Money -> Map AccountId Money
addMoneyToAccount accId money accounts = let
    balance = moneyInAccount accId accounts
    newBalance = balance + money
    in if Ada.getLovelace money <= 0 then accounts
    else updateMoneyInAccount accId newBalance accounts


{-| Gives the given amount of money to the given payee.
    Returns the appropriate effect and updated accounts
-}
{-# INLINABLE giveMoney #-}
giveMoney :: Payee -> Money -> Map AccountId Money -> (ReduceEffect, Map AccountId Money)
giveMoney payee money accounts = case payee of
    Party party   -> (ReduceWithPayment (Payment party money), accounts)
    Account accId -> let
        newAccs = addMoneyToAccount accId money accounts
        in (ReduceNoPayment, newAccs)


-- | Carry a step of the contract with no inputs
{-# INLINABLE reduceContractStep #-}
-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of

    Refund -> case refundOne (accounts state) of
        Just ((party, money), newAccounts) -> let
            newState = state { accounts = newAccounts }
            in Reduced ReduceNoWarning (ReduceWithPayment (Payment party money)) newState Refund
        Nothing -> NotReduced

    Pay accId payee val cont -> let
        amountToPay = evalValue env state val
        in  if amountToPay <= 0
            then Reduced (ReduceNonPositivePay accId payee amountToPay) ReduceNoPayment state cont
            else let
                balance    = moneyInAccount accId (accounts state) -- always positive
                moneyToPay = Ada.lovelaceOf amountToPay -- always positive
                paidMoney  = min balance moneyToPay -- always positive
                newBalance = balance - paidMoney -- always positive
                newAccs    = updateMoneyInAccount accId newBalance (accounts state)
                warning = if paidMoney < moneyToPay
                          then ReducePartialPay accId payee paidMoney moneyToPay
                          else ReduceNoWarning
                (payment, finalAccs) = giveMoney payee paidMoney newAccs
                in Reduced warning payment (state { accounts = finalAccs }) cont

    If obs cont1 cont2 -> let
        cont = if evalObservation env state obs then cont1 else cont2
        in Reduced ReduceNoWarning ReduceNoPayment state cont

    When _ timeout cont -> let
        startSlot = fst (slotInterval env)
        endSlot   = snd (slotInterval env)
        -- if timeout in future – do not reduce
        in if endSlot < timeout then NotReduced
        -- if timeout in the past – reduce to timeout continuation
        else if timeout <= startSlot then Reduced ReduceNoWarning ReduceNoPayment state cont
        -- if timeout in the slot range – issue an ambiguity error
        else AmbiguousSlotIntervalReductionError

    Let valId val cont -> let
        evaluatedValue = evalValue env state val
        boundVals = boundValues state
        newState = state { boundValues = Map.insert valId evaluatedValue boundVals }
        warn = case Map.lookup valId boundVals of
              Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
              Nothing -> ReduceNoWarning
        in Reduced warn ReduceNoPayment newState cont


-- | Reduce a contract until it cannot be reduced more
{-# INLINABLE reduceContractUntilQuiescent #-}
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent env state contract = let
    reductionLoop
      :: Environment -> State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop env state contract warnings payments =
        case reduceContractStep env state contract of
            Reduced warning effect newState cont -> let
                newWarnings = if warning == ReduceNoWarning then warnings
                              else warning : warnings
                newPayments  = case effect of
                    ReduceWithPayment payment -> payment : payments
                    ReduceNoPayment -> payments
                in reductionLoop env newState cont newWarnings newPayments
            AmbiguousSlotIntervalReductionError -> RRAmbiguousSlotIntervalError
            -- this is the last invocation of reductionLoop, so we can reverse lists
            NotReduced -> ContractQuiescent (reverse warnings) (reverse payments) state contract

    in reductionLoop env state contract [] []


-- Apply a single Input to the contract (assumes the contract is reduced)
{-# INLINABLE applyCases #-}
applyCases :: Environment -> State -> Input -> [Case] -> ApplyResult
applyCases env state input cases = case (input, cases) of
    (IDeposit accId1 party1 money, (Deposit accId2 party2 val, cont) : rest) -> let
        amount = evalValue env state val
        newState = state { accounts = addMoneyToAccount accId1 money (accounts state) }
        in if accId1 == accId2 && party1 `eqPubKey` party2 && Ada.getLovelace money == amount
        then Applied newState cont
        else applyCases env state input rest
    (IChoice choId1 choice, (Choice choId2 bounds, cont) : rest) -> let
        newState = state { choices = Map.insert choId1 choice (choices state) }
        in if choId1 == choId2 && inBounds choice bounds
        then Applied newState cont
        else applyCases env state input rest
    (INotify, (Notify obs, cont) : _) | evalObservation env state obs -> Applied state cont
    (_, _ : rest) -> applyCases env state input rest
    (_, []) -> ApplyNoMatchError


{-# INLINABLE applyInput #-}
applyInput :: Environment -> State -> Input -> Contract -> ApplyResult
applyInput env state input (When cases _ _) = applyCases env state input cases
applyInput _ _ _ _                          = ApplyNoMatchError


-- | Apply a list of Inputs to the contract
{-# INLINABLE applyAllInputs #-}
applyAllInputs :: Environment -> State -> Contract -> [Input] -> ApplyAllResult
applyAllInputs env state contract inputs = let
    applyAllLoop
        :: Environment
        -> State
        -> Contract
        -> [Input]
        -> [ReduceWarning]
        -> [Payment]
        -> ApplyAllResult
    applyAllLoop env state contract inputs warnings payments =
        case reduceContractUntilQuiescent env state contract of
            RRAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
            ContractQuiescent warns pays curState cont -> case inputs of
                [] -> ApplyAllSuccess (warnings ++ warns) (payments ++ pays) curState cont
                (input : rest) -> case applyInput env curState input cont of
                    Applied newState cont ->
                        applyAllLoop env newState cont rest (warnings ++ warns) (payments ++ pays)
                    ApplyNoMatchError -> ApplyAllNoMatchError
    in applyAllLoop env state contract inputs [] []



type TransactionSignatures = Map Party Bool

-- | Extract necessary signatures from transaction inputs
{-# INLINABLE getSignatures #-}
getSignatures :: [Input] -> Map Party Bool
getSignatures = foldl addSig (Map.empty())
  where
    addSig acc (IDeposit _ p _)           = Map.insert p True acc
    addSig acc (IChoice (ChoiceId _ p) _) = Map.insert p True acc
    addSig acc INotify                    = acc


-- | Try to compute outputs of a transaction give its input
{-# INLINABLE computeTransaction #-}
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract = let
    inputs = txInputs tx
    in case fixInterval (txInterval tx) state of
        IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
            ApplyAllSuccess warnings payments newState cont -> let
                in  if contract == cont
                    then Error TEUselessTransaction
                    else TransactionOutput { txOutWarnings = warnings
                                           , txOutPayments = payments
                                           , txOutState = newState
                                           , txOutContract = cont }
            ApplyAllNoMatchError -> Error TEApplyNoMatchError
            ApplyAllAmbiguousSlotIntervalError -> Error TEAmbiguousSlotIntervalError
        IntervalError error -> Error (TEIntervalError error)


-- | Calculates an upper bound for the maximum lifespan of a contract
{-# INLINABLE contractLifespanUpperBound #-}
contractLifespanUpperBound :: Contract -> Integer
contractLifespanUpperBound contract = case contract of
    Refund -> 0
    Pay _ _ _ cont -> contractLifespanUpperBound cont
    If _ contract1 contract2 ->
        max (contractLifespanUpperBound contract1) (contractLifespanUpperBound contract2)
    When cases timeout subContract -> let
        contractsLifespans = fmap (\(_, cont) -> contractLifespanUpperBound cont) cases
        in maximum (getSlot timeout : contractLifespanUpperBound subContract : contractsLifespans)
    Let _ _ cont -> contractLifespanUpperBound cont

{-# INLINABLE validatePayments #-}
validatePayments :: PendingTx -> [Payment] -> Integer -> Bool
validatePayments pendingTx txOutPayments scriptOutput = let
    -- TxIn we're validating is obviously a Script TxIn.
    PendingTxIn _ _ scriptInValue = pendingTxIn pendingTx

    scriptInAdaValue = Ada.fromValue scriptInValue
    in False


{-# INLINABLE validateContinuation #-}
validateContinuation :: PendingTx -> DataScriptHash -> Maybe Integer
validateContinuation pendingTx (DataScriptHash dsHash) = let
    -- TODO FIXME use Validation.getContinuingOutputs
    getContinuingOutputs :: PendingTx -> [PendingTxOut]
    getContinuingOutputs PendingTx{pendingTxIn=PendingTxIn{pendingTxInWitness=Just(ValidatorHash inpHsh, _)}, pendingTxOutputs=outs} = filter f outs
        where
            f PendingTxOut{pendingTxOutHashes=(Just (ValidatorHash outHsh, _))} = outHsh == inpHsh
            f _ = False
    -- Not spending a script output
    getContinuingOutputs _ = []

    -- lookup for a validator script with the same hash (i.e. Marlowe continuation contract)
    in case getContinuingOutputs pendingTx of
        {-  It is *not* okay to have multiple outputs with the current validator script,
            that allows "spliting" the Marlowe Contract. -}
        [PendingTxOut outValue (Just (_, DataScriptHash dsh)) DataTxOut]
            | Builtins.equalsByteString dsh dsHash -> (Just . Ada.getLovelace . Ada.fromValue) outValue
        _ -> Nothing


{-|
    Marlowe Interpreter ValidatorScript generator.
-}
{-# INLINABLE mkValidator #-}
mkValidator
  :: PubKey -> MarloweData -> ([Input], Sealed (HashedDataScript MarloweData)) -> PendingTx -> Bool
mkValidator creator MarloweData{..} (inputs, sealedMarloweData) pendingTx@PendingTx{..} = let
    HashedDataScript (MarloweData _ expectedState expectedContract) dsHash = unseal sealedMarloweData
    {-  Embed contract creator public key. This makes validator script unique,
        which makes a particular contract to have a unique script address.
        That makes it easier to watch for contract actions inside a wallet. -}
    checkCreator =
        if marloweCreator `eqPubKey` creator then True
        else traceErrorH "Wrong contract creator"

    {-  We require Marlowe Tx to have both lower bound and upper bounds in 'SlotRange'.
    -}
    (minSlot, maxSlot) = case pendingTxValidRange of
        Interval (Just l)  (Just (Slot h)) -> (l, Slot (h - 1))
        _ -> traceErrorH "Tx valid slot must have lower bound and upper bounds"

    slotInterval = (minSlot, maxSlot)
    txInput = TransactionInput { txInterval = slotInterval, txInputs = inputs }
    result = computeTransaction txInput marloweState marloweContract
    in case result of
        TransactionOutput {txOutPayments, txOutState, txOutContract} ->
            if txOutContract == Refund
            -- if it's a last transaction, don't expect any continuation, everything is payed out.
            then checkCreator && validatePayments pendingTx txOutPayments 0
            -- otherwise check the continuation
            else case validateContinuation pendingTx dsHash of
                Just scriptOutput -> let
                    validPayments = validatePayments pendingTx txOutPayments scriptOutput
                    validContract = txOutState == expectedState && txOutContract == expectedContract
                    in checkCreator && validPayments && validContract
                Nothing -> False
        Error _ -> traceErrorH "Error"


validatorScript :: PubKey -> ValidatorScript
validatorScript creator = ValidatorScript $
    $$(Ledger.compileScript [|| mkValidator ||]) `Ledger.applyScript` Ledger.lifted creator
