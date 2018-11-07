\documentclass[11pt,a4paper]{article}
\usepackage{tikz}
\usepackage[legalpaper,margin=1in]{geometry}
\usetikzlibrary{positioning}
%include polycode.fmt
\begin{document}

\begin{code}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Marlowe.Compiler where

import qualified Data.List                           as List
import qualified Data.Set                           as Set
import Data.Set                           (Set)
import qualified Data.Map.Strict                           as Map
import Data.Map.Strict                           (Map)

import qualified Language.Plutus.CoreToPLC.Builtins as Builtins
import           Language.Plutus.Runtime            (PendingTx (..), PendingTxIn (..), PendingTxOut (..), PubKey (..), Value)
import           Language.Plutus.TH                 (plutus)
import           Wallet.UTXO                        (Address', DataScript (..), TxOutRef', Validator (..), scriptTxIn,
                                                        scriptTxOut, applyScript)
import qualified Wallet.UTXO                        as UTXO

import qualified Language.Plutus.Runtime.TH         as TH
import           Prelude                            (Integer, Bool (..), Num (..), Show(..), Read(..), Ord (..), Eq (..),
                    fromIntegral, succ, sum, ($), (<$>), (++), otherwise)

\end{code}

\section{Marlowe}
\begin{code}
data Contract = Null
              | CommitCash IdentCC PubKey Value Timeout Timeout Contract Contract
              | Pay IdentPay Person Person Value Timeout Contract
              | Both Contract Contract
                deriving (Eq)
\end{code}

Assumptions
\begin{itemize}
\item Fees are payed by transaction issues. For simplicity, assume zero fees.
\item PubKey is actually a hash of a public key
\item Every contract is created by contract owner by issuing a transaction with the contract in TxOut
\end{itemize}

\begin{spec}
example = CommitCash (IdentCC 1) (PubKey 1) (Value 100) (Block 200) (Block 256)
            (Pay (IdentPay 1) (PubKey 1) (PubKey 2) (Value 100) (RedeemCC (IdentCC 1) Null))
            Null
\end{spec}

\section{Questions}

Q: - Should we put together the first CommitCash with the Contract setup? Contract setup would still require some money.

Q: - Should we be able to return excess money in the contract (money not accounted for). To whom?
  We could use excess money to ensure a contract has money on it, and then return to the creator of the contract when it becomes Null.

Q: - There is a risk someone will put a continuation of a Marlowe contract without getting the previous continuation as input.
  Can we detect this and allow for refund?

Q: - What happens on a FailedPay? Should we still pay what we can?

Q: - What is signed in a transaction?


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
Someone need to spend this 1 Ada, otherwise all Marlowe contracts will be in UTXO.
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

Contract execution is a chain of transactions, where contract state is passed through dataScript,
and actions/inputs are passed as a redeemer script and TxIns/TxOuts

validation script =  marlowe interpreter + contract

redeemer script = action/input, i.e. CommitCash val timeout, Choice 1, OracleValue "oil" 20

pendingTx

dataScript = input/observation, e.g. CashCommit, Choice


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

\begin{code}

type Timeout = Integer
type Cash = Value

type Inputs = [CC]

type Person      = PubKey


contractPlcCode = $(plutus [| CommitCash (IdentCC 1) (PubKey 1) 123 100 200 Null Null |])

-- Commitments, choices and payments are all identified by identifiers.
-- Their types are given here. In a more sophisticated model these would
-- be generated automatically (and so uniquely); here we simply assume that
-- they are unique.

newtype IdentCC = IdentCC Integer
               deriving (Eq,Ord)

newtype IdentChoice = IdentChoice { unIdentChoice :: Integer }
               deriving (Eq,Ord)

newtype IdentPay = IdentPay Integer
               deriving (Eq,Ord)

-- A cash commitment is made by a person, for a particular amount and timeout.

data CC = CC IdentCC Person Cash Timeout
               deriving (Eq,Ord)

-- A cash redemption is made by a person, for a particular amount.

data RC = RC IdentCC Person Cash
               deriving (Eq,Ord)

data Input = Input {
                cc  :: Set.Set CC,
                rc  :: Set.Set RC,
                rp  :: Map.Map (IdentPay, Person) Cash
            }

emptyInput :: Input
emptyInput = Input Set.empty Set.empty Map.empty



data State = State {
                sc  :: Map.Map IdentCC CCStatus,
                sch :: Map.Map (IdentChoice, Person) ConcreteChoice
            } deriving (Eq,Ord)

data MarloweState = MarloweState {
        marloweInput :: Input,
        marloweState :: State,
        marloweContract :: Contract
    }

data OS =  OS {  blockNumber  :: Integer }
                    deriving (Eq,Ord)

type ConcreteChoice = Integer

data Action =   SuccessfulPay IdentPay Person Person Cash |
                ExpiredPay IdentPay Person Person Cash |
                FailedPay IdentPay Person Person Cash |
                SuccessfulCommit IdentCC Person Cash |
                CommitRedeemed IdentCC Person Cash |
                ExpiredCommitRedeemed IdentCC Person Cash |
                DuplicateRedeem IdentCC Person |
                ChoiceMade IdentChoice Person ConcreteChoice
                    deriving (Eq,Ord)

type AS = [Action]


type CCStatus = (Person,CCRedeemStatus)
data CCRedeemStatus = NotRedeemed Cash Timeout | ManuallyRedeemed
               deriving (Eq,Ord)


marloweValidator :: UTXO.Script -> Validator
marloweValidator contractPlcCode = Validator result where
    result = applyScript inner contractPlcCode
    -- result = inner
    inner = UTXO.fromPlcCode $(plutus [| \contract redeemer (pendingTx :: PendingTx) dataScript ->
        let
            state = dataScript
            inputCommand = redeemer

            txInputs :: [PendingTxIn]
            txInputs = pendingTxInputs pendingTx

            txOutputs     :: [PendingTxOut]
            txOutputs = pendingTxOutputs pendingTx

            input :: Input
            input = emptyInput

            os = OS { blockNumber = fromIntegral blockHeight }

            blockHeight = pendingTxBlockHeight pendingTx

            validInput = True

            validOutput = True

            infixr 3 &&
            (&&) :: Bool -> Bool -> Bool
            (&&) = $(TH.and)

            step :: Input -> State -> Contract -> OS -> (State,Contract,AS)
            step _ st Null _ = (st, Null, [])

            stepAll :: Input -> State -> Contract -> OS -> (State, Contract, AS)
            stepAll com st con os = stepAllAux com st con os []

            stepAllAux :: Input -> State -> Contract -> OS -> AS -> (State, Contract, AS)
            stepAllAux com st con os ac
                | (nst == st) && (ncon == con) && List.null nac = (st, con, ac)
                | otherwise = stepAllAux com nst ncon os (nac ++ ac)
                where
                    (nst, ncon, nac) = step com st con os

            -- Defines if expiry time ee has come if current time is e
            expired :: Timeout -> Timeout -> Bool
            expired e ee = ee <= e

            (newState, newContract, actions) = stepAll input state contract os

            goodAction action = case action of
                SuccessfulPay{} -> True
                ExpiredPay{} -> False
                FailedPay{} -> False
                SuccessfulCommit{} -> True
                CommitRedeemed{} -> True
                ExpiredCommitRedeemed{} -> True
                DuplicateRedeem{} -> False
                ChoiceMade{} -> True
            areAllActionsGood = List.all goodAction actions

            isValid = validInput && validOutput && areAllActionsGood
        in if isValid then () else Builtins.error ()
        |])
\end{code}
\end{document}