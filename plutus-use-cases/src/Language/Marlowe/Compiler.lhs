\documentclass[11pt,a4paper]{article}
\usepackage{tikz}
\usepackage[legalpaper,margin=1in]{geometry}
\usetikzlibrary{positioning}
%include polycode.fmt
\begin{document}

\begin{code}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE StrictData    #-} -- doesn't work yet with current Plutus Compiler
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin -fplugin-opt Language.Plutus.CoreToPLC.Plugin:dont-typecheck #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Marlowe.Compiler where
import           Control.Applicative        (Applicative (..))
import           Control.Monad              (Monad (..))
import           Control.Monad.Error.Class  (MonadError (..))
import           GHC.Generics               (Generic)
import qualified Data.List                           as List
import qualified Data.Set                           as Set
import Data.Set                           (Set)
import qualified Data.Map.Strict                           as Map
import Data.Map.Strict                           (Map)

import qualified Language.Plutus.CoreToPLC.Builtins as Builtins
import           Language.Plutus.Runtime
import           Language.Plutus.TH                 (plutus)
import           Wallet.API                 (EventTrigger (..), Range (..), WalletAPI (..), WalletAPIError, otherError,
                                             pubKey, signAndSubmit, payToPubKey, ownPubKeyTxOut)

import           Wallet.UTXO                        (Address', DataScript (..), TxOutRef', TxOut', TxOut(..), Validator (..), scriptTxIn,
                                                        scriptTxOut, applyScript, emptyValidator, unitData, txOutValue)
import qualified Wallet.UTXO                        as UTXO

import qualified Language.Plutus.Runtime.TH         as TH
import           Language.Plutus.Lift       (LiftPlc (..), TypeablePlc (..))
import           Prelude                            (Int, Integer, Bool (..), Num (..), Show(..), Read(..), Ord (..), Eq (..),
                    fromIntegral, succ, sum, ($), (<$>), (++), otherwise, Maybe(..))

\end{code}

\section{Marlowe}

Apparently, Plutus doesn't support complex recursive data types yet.


\begin{code}


-- data C1 = Null1 | Pay Person Person Value Timeout deriving (Eq, Generic)

data Contract = Null
              | CommitCash IdentCC PubKey Value Timeout Timeout Contract
                deriving (Eq, Generic)
type C1 = Contract





\end{code}

\section{Assumptions}

\begin{itemize}
\item Fees are payed by transaction issues. For simplicity, assume zero fees.
\item PubKey is actually a hash of a public key
\item Every contract is created by contract owner by issuing a transaction with the contract in TxOut
\end{itemize}


\sectoin{Examples}

\begin{spec}
Alice = (PubKey 1)
Bob   = (PubKey 2)
value = (Value 100)
example = CommitCash (IdentCC 1) Alice value (Block 200) (Block 256)
            (Pay (IdentPay 1) Alice Bob value (Block 256) Null)
            Null
\end{spec}


\section{Questions}


Q: Should we put together the first CommitCash with the Contract setup? Contract setup would still require some money.

Q: Should we be able to return excess money in the contract (money not accounted for). To whom?
  We could use excess money to ensure a contract has money on it, and then return to the creator of the contract when it becomes Null.

Q: There is a risk someone will put a continuation of a Marlowe contract without getting the previous continuation as input.
  Can we detect this and allow for refund?

Q: What happens on a FailedPay? Should we still pay what we can?

Q: What is signed in a transaction?

Q: How to distinguish different instances of contracts? Is it a thing?
    Maybe we need to add a sort of identifier of a contract.



\begin{itemize}
\item Whole validator script (read Contract script) on every spending tx.
\item No offchain messages (`internal messages` in Ethereum)? How to call a function?
Answer: currently only via transaction
\end{itemize}


\section{Contract Initialization} \label{ContractInit}

This can be done in 2 ways.


\subsection{Initialization by depositing Ada to a new contract}

Just pay 1 Ada to a contract so that it becomes a part of UTXO.

\begin{tikzpicture}[
squarednode/.style={rectangle, draw=orange!60, fill=orange!5, very thick, minimum size=10mm},
]
%Nodes
\node[squarednode] (commitcash) {TxIn Alice 1000};
\node[squarednode,align=center] (txOut1)       [right=of commitcash]
    {Contract\\value = 1\\dataScript = State \{\}};
\node[squarednode] (txOut2)       [below=of txOut1]
    {Change AliceAddr 999};

%Lines
\draw[->,thick] (commitcash.east) -- (txOut1.west);
\draw[->,thick] (commitcash.south) -- (txOut2.west);
\end{tikzpicture}


\par{Considerations}
Someone need to spend this 1 Ada/Lovelace, otherwise all Marlowe contracts will be in UTXO.
We can allow anyone to spend this value, so it'll become a part of a block reward. ???


\subsection{Initialization by CommitCash}

Any contract that starts with CommitCash can be initialized with actuall CommitCash

\begin{tikzpicture}[
squarednode/.style={rectangle, draw=orange!60, fill=orange!5, very thick, minimum size=10mm},
]
%Nodes
\node[squarednode] (commitcash) {TxIn Alice 1000};
\node[squarednode,align=center] (txOut1)       [right=of commitcash]
    {Contract\\value = 100\\dataScript = State \{\\commits = [Committed #1 Alice v:100 t:256]\}};
\node[squarednode] (txOut2)       [below=of txOut1]
    {Change AliceAddr 900};

%Lines
\draw[->,thick] (commitcash.east) -- (txOut1.west);
\draw[->,thick] (commitcash.south) -- (txOut2.west);
\end{tikzpicture}



\section{Semantics}

Contract execution is a chain of transactions, where contract state is passed through \emph{dataScript},
and actions/inputs are passed as a \emph{redeemer} script and TxIns/TxOuts

Validation Script =  marlowe interpreter + possibly encoded address of a contract owner for initial deposit refund

This would change script address for every contract owner. This could be a desired or not desired property. Discuss.

redeemer script = action/input, i.e. CommitCash val timeout, Choice 1, OracleValue "oil" 20

pendingTx

dataScript = Contract + State

This implies that remaining Contract and its State are publicly visible. Discuss.


\subsection{Null}
Possibly allow redeem of cash spent by mistake on this address? How?

If we have all chain of txs of a contract we could allow redeems of mistakenly put money,
and that would allow a contract creator to withdraw the contract initialization payment. \ref{ContractInit}


\subsection{CommitCash}

Alice has 1000 ADA in AliceUTXO.

\begin{tikzpicture}[
squarednode/.style={rectangle, draw=orange!60, fill=orange!5, very thick, minimum size=10mm},
]
%Nodes
\node[squarednode,align=center] (contract) {Contract\\redeemer = CC #1 v:100 t:256};
\node[squarednode] (commitcash)  [below=of contract]
    {TxIn Alice 1000};
\node[squarednode,align=center] (txOut1)       [right=of contract]
    {Contract'\\value = 100\\dataScript = State \{\\commits = [Committed #1 Alice v:100 t:256]\}};
\node[squarednode] (txOut2)       [below=of txOut1]
    {Change AliceAddr 900};

%Lines
\draw[->,thick] (contract.east) -- (txOut1.west);
\draw[->,thick] (commitcash.east) -- (txOut1.south);
\draw[->,thick] (commitcash.east) -- (txOut2.west);
\end{tikzpicture}


\subsection{RedeemCC}

Redeem a previously make CommitCash if valid.
Alice committed 100 ADA with CC 1, timeout 256.

\begin{tikzpicture}[
squarednode/.style={rectangle, draw=orange!60, fill=orange!5, very thick, minimum size=10mm},
]
%Nodes
\node[squarednode,align=center] (contract) {Contract\\redeemer = RC 1};
\node[squarednode,align=center] (txOut1)       [right=of contract]
    {Contract'\\dataScript = State \{commits = []\}};
\node[squarednode] (txOut2)       [below=of txOut1]
    {Change AliceAddr 900};

%Lines
\draw[->,thick] (contract.east) -- (txOut1.west);
\draw[->,thick] (contract.east) -- (txOut2.west);
\end{tikzpicture}


\subsection{Pay}

Alice pays 100 ADA to Bob.

\begin{tikzpicture}[
squarednode/.style={rectangle, draw=orange!60, fill=orange!5, very thick, minimum size=10mm},
]
%Nodes
\node[squarednode,align=center] (contract) {Contract\\redeemer = Pay AliceSignature BobAddress v:100};
\node[squarednode,align=center] (txOut1)       [right=of contract]
    {Contract'\\dataScript = State \{commits - payment\}};
\node[squarednode] (txOut2)       [below=of txOut1] {BobAddress 100};

%Lines
\draw[->,thick] (contract.east) -- (txOut1.west);
\draw[->,thick] (contract.south) -- (txOut2.west);
\end{tikzpicture}



\section{Types and Data Representation}

\begin{code}

type Timeout = Height
type Cash = Value

type Person      = PubKey


-- contractPlcCode = $(plutus [| CommitCash (IdentCC 1) (PubKey 1) 123 100 200 Null Null |])

-- Commitments, choices and payments are all identified by identifiers.
-- Their types are given here. In a more sophisticated model these would
-- be generated automatically (and so uniquely); here we simply assume that
-- they are unique.

newtype IdentCC = IdentCC Int
               deriving (Eq, Ord, Generic)
instance LiftPlc IdentCC
instance TypeablePlc IdentCC


newtype IdentChoice = IdentChoice { unIdentChoice :: Int }
               deriving (Eq, Ord, Generic)
instance LiftPlc IdentChoice
instance TypeablePlc IdentChoice

newtype IdentPay = IdentPay Int
               deriving (Eq, Ord, Generic)
instance LiftPlc IdentPay
instance TypeablePlc IdentPay


-- A cash commitment is made by a person, for a particular amount and timeout.

data CC = CC IdentCC Person Cash Timeout
               deriving (Eq, Ord, Generic)
instance LiftPlc CC
instance TypeablePlc CC


-- A cash redemption is made by a person, for a particular amount.

data RC = RC IdentCC Person Cash
               deriving (Eq, Ord, Generic)
instance LiftPlc RC
instance TypeablePlc RC


data Input = Commit CC | SpendDeposit deriving (Generic)
instance LiftPlc Input
instance TypeablePlc Input


data State = State {
                stateCommitted  :: [(IdentCC, CCStatus)]
            } deriving (Eq, Ord, Generic)
instance LiftPlc State
instance TypeablePlc State


emptyState :: State
emptyState = State {stateCommitted = []}


data MarloweData = MarloweData {
        marloweState :: State,
        marloweContract :: Contract
    } deriving (Generic)
instance LiftPlc MarloweData
instance TypeablePlc MarloweData

type ConcreteChoice = Int

type CCStatus = (Person, CCRedeemStatus)

data CCRedeemStatus = NotRedeemed Cash Timeout | ManuallyRedeemed
               deriving (Eq, Ord, Generic)
instance LiftPlc CCRedeemStatus
instance TypeablePlc CCRedeemStatus

-- instance LiftPlc C1
-- instance TypeablePlc C1
instance LiftPlc Contract
instance TypeablePlc Contract
\end{code}



\section{Marlowe Interpreter and Helpers}

\begin{code}
marloweValidator = Validator result where
    result = UTXO.fromPlcCode $(plutus [| \ (input :: Input) (MarloweData{..} :: MarloweData) (p :: PendingTx ValidatorHash) -> let
        -- let isValid = True

        eqPk :: PubKey -> PubKey -> Bool
        eqPk = $(TH.eqPubKey)

        infixr 3 &&
        (&&) :: Bool -> Bool -> Bool
        (&&) = $(TH.and)

        infixr 3 ||
        (||) :: Bool -> Bool -> Bool
        (||) = $(TH.or)

        orderTxIns :: PendingTxIn -> PendingTxIn -> (PendingTxIn, PendingTxIn)
        orderTxIns t1 t2 = case t1 of
            PendingTxIn _ (Just _ :: Maybe (ValidatorHash, RedeemerHash)) _ -> (t1, t2)
            _ -> (t2, t1)

        extract2x2 :: PendingTx ValidatorHash -> a -> ((Height, PendingTxIn, PendingTxIn, PendingTxOut, PendingTxOut) -> a) -> a
        extract2x2 p def f = case p of
            PendingTx (ins::[PendingTxIn]) (outs::[PendingTxOut]) _ _ blockNumber _ _ -> case (ins, outs) of
                ((in1::PendingTxIn):(ins'::[PendingTxIn]) , (out1::PendingTxOut):(outs'::[PendingTxOut])) ->
                    case (ins', outs') of
                        ((in2::PendingTxIn):(_::[PendingTxIn]) , (out2::PendingTxOut):(_::[PendingTxOut])) ->
                            case orderTxIns in1 in2 of
                                (i1, i2) -> f (blockNumber, i1, i2, out1, out2)
                        _ -> def
                _ -> def


        step :: Input -> State -> Contract -> (State, C1 , Bool)
        step input state contract = case marloweContract of
            CommitCash (IdentCC expectedIdentCC) pubKey value startTimeout endTimeout c1 -> case input of
                Commit (CC (IdentCC idCC) (person::Person) (v::Cash) (t::Timeout)) ->
                    extract2x2 p (state, Null, False) (\(blockNumber, PendingTxIn _ _ scriptValue, PendingTxIn _ _ commitValue, out1, out2) ->
                        case out1 of
                            PendingTxOut committed (Just (validatorHash, dataHash)) DataTxOut -> let
                                isValid = blockNumber <= startTimeout
                                    && blockNumber <= endTimeout
                                    && committed == v + scriptValue
                                    && expectedIdentCC == idCC
                                    && pubKey `eqPk` person
                                    && endTimeout == t
                                    -- TODO check hashes
                                in  if isValid then let
                                        cns = (person, NotRedeemed commitValue endTimeout)
                                        updatedState = case state of { State stateCommitted -> State ((IdentCC idCC, cns) : stateCommitted) }
                                        in (updatedState, c1, True)
                                    else (state, c1, False)
                            _ -> (state, Null, False))
                SpendDeposit -> (state, Null, False)
            Null -> case input of
                SpendDeposit -> (state, Null, True)
                _ -> (state, Null, False)

        (newState::State, newContract::C1, isValid::Bool) = step input marloweState marloweContract

        in if isValid then () else Builtins.error ()
        |])


createContract :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Contract
    -> Value
    -> m ()
createContract contract value = do
    _ <- if value <= 0 then otherError "Must contribute a positive value" else pure ()
    let ds = DataScript $ UTXO.lifted MarloweData { marloweContract = contract, marloweState = emptyState }
    let v' = UTXO.Value $ fromIntegral value
    (payment, change) <- createPaymentWithChange v'
    let o = scriptTxOut v' marloweValidator ds

    signAndSubmit payment [o, change]


commitCash :: (
    MonadError WalletAPIError m,
    WalletAPI m)
    => Person
    -> (TxOut', TxOutRef')
    -> Integer
    -> Timeout
    -> m ()
commitCash person (TxOut _ (UTXO.Value contractValue) _, ref) value timeout = do
    _ <- if value <= 0 then otherError "Must commit a positive value" else pure ()
    let identCC = (IdentCC 1)
    let i   = scriptTxIn ref marloweValidator (UTXO.Redeemer $ UTXO.lifted $ Commit (CC identCC person (fromIntegral value) timeout))

    let ds = DataScript $ UTXO.lifted (MarloweData {
                marloweContract = Null,
                marloweState = State {stateCommitted=[(identCC, (person, NotRedeemed (fromIntegral value) timeout))]} })
    (payment, change) <- createPaymentWithChange (UTXO.Value value)
    let o = scriptTxOut (UTXO.Value $ value + contractValue) marloweValidator ds

    signAndSubmit (Set.insert i payment) [o, change]



endContract :: (Monad m, WalletAPI m) => (TxOut', TxOutRef') -> m ()
endContract (TxOut _ val _, ref) = do
    oo <- ownPubKeyTxOut val
    let scr = marloweValidator
        i   = scriptTxIn ref scr $ UTXO.Redeemer $ UTXO.lifted $ SpendDeposit
    signAndSubmit (Set.singleton i) [oo]


\end{code}
\end{document}