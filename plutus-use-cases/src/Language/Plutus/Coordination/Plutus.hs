-- | This is a mock of (parts of) the Plutus API
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
-- | A model of the types involved in transactions, and of the wallet API.
module Language.Plutus.Coordination.Plutus (-- * Transactions and related types
                Address
              , PubKey(..)
              , KeyPair
              , pubKey
              , Value
              , Tx(..)
              , TxIn(..)
              , TxOut(..)
              , mkAddress
              , TxOutRef(..)
              , standardTxFee
              , txOutValue
              , txOutDataScript
              , txOutValidatorScriptHash
              -- * API operations
              , TxM
              , Hash
              , hash
              , Redeemer
              , Validator
              , DataScript
              , PlutusTx(..)
              , BlockHeight
              , PendingTx(..)
              , submitTransaction
              , assert
              , lookupMyKeyPair
              , lookupMyPubKey
              , createPayment
              , txInSign
              , Range(..)
              , EventTrigger(..)
              , Signed(..)
              , OracleValue(..)
              ) where

import           Control.Applicative              (Alternative (..))
import           Control.Monad.State              (State)
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Language.Plutus.CoreToPLC.Plugin (PlcCode)

newtype Signed a = Signed (PubKey, a)

-- `(pk, (bh, i, a)) :: OracleValue a` is the value of an "observable" `i`
-- (sensor, etc.) observed at `bh` signed by an oracle `pk`.
--
newtype OracleValue a = OracleValue (Signed (BlockHeight, Int, a))

-- | Cardano address
--
newtype Address = Address Int
    deriving (Eq, Ord, Show, Read)

-- | Ada value
--
type Value = Int

newtype Hash = Hash Int
    deriving (Eq, Ord, Show, Read)

hash :: a -> Hash
hash _ = Hash 10

-- | Public key
--
data PubKey = PubKey

-- | Public key pair (no lift instance, because we never ought to put it into a
--   transaction)
--
data KeyPair

data TxState

-- | Transaction monad for coordination layer computations; provides access to
-- the blockchain
--
type TxM a = MaybeT (State TxState) a

-- | Submit the given transaction to the blockchain
submitTransaction :: Tx -> TxM [TxOutRef]
submitTransaction = const empty

-- | Verify that a condition is true.
assert :: Bool -> TxM ()
assert = const empty

-- | Get the users's public key. Part of the wallet interface
lookupMyPubKey :: TxM PubKey
lookupMyPubKey = pubKey <$> lookupMyKeyPair

-- | Extract the public key from a key pair.
pubKey :: KeyPair -> PubKey
pubKey = const PubKey

-- | Part of the wallet interface
--   TODO: Should the Plutus client even be able to know the private key?
lookupMyKeyPair :: TxM KeyPair
lookupMyKeyPair = empty

-- | Create an input that spends the given value (part of the wallet interface)
--
createPayment :: Value -> TxM TxIn
createPayment = const empty

-- | A UTxO transaction specification
--
data Tx = Tx
          { txInputs  :: (TxIn, TxIn)
          , txOutputs :: (TxOut Int, TxOut Int)
          }

-- | UTxO input
--
data TxIn = TxIn
            { txInOutRef   :: !TxOutRef
            , txInRedeemer :: !Hash
            }

-- | Construct an input that can spend the given output (assuming it was payed
--   to an address in our wallet.) Part of the wallet interface
--
txInSign :: TxOutRef -> KeyPair -> TxIn
txInSign to _ = TxIn to (Hash 10)

-- | Reference to an unspent output
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#extension-to-transaction-outputs
--
data TxOutRef =
  TxOutRef
  {
     txOutRefValue          :: !Value -- We assume this is added by the library. TODO: In cardano-sl this is a "ValueDistribution" (map of keys to values)
   , txOutRefValidatorHash  :: !Hash -- Hash of validator script. The validator script has to be submitted by the consumer of the outputs referenced by this TxOutRef.
   , txOutRefDataScriptHash :: !Hash -- Hash of data script used by the creator of the transaction.
  }

type BlockHeight = Int

-- | Information about a pending transaction used by validator scripts.
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#blockchain-state-available-to-validator-scripts
data PendingTx = PendingTx {
      pendingTxBlockHeight :: !BlockHeight -- ^ Block height exl. current transaction
    , pendingTxHash        :: !Hash -- ^ Hash of the transaction that is being validated
    , pendingTxTransaction :: !Tx
    }

-- | UTxO output
--
data TxOut a = TxOutPubKey  !Value !PubKey
           | TxOutScript  !Value !Hash !a

-- | An address in cardano is a hash of the information in `TxOut`
mkAddress :: TxOut a -> Address
mkAddress = const (Address 5)

txOutValue :: TxOut a -> Value
txOutValue = \case
    TxOutPubKey v _ -> v
    TxOutScript v _ _ -> v

txOutDataScript :: TxOut a -> Maybe a
txOutDataScript = \case
    TxOutScript _ _ r -> Just r
    _ -> Nothing

txOutValidatorScriptHash :: TxOut a -> Maybe Hash
txOutValidatorScriptHash = \case
    TxOutScript _ h _ -> Just h
    _ -> Nothing

-- | PlutusTx code
--
newtype PlutusTx = PlutusTx { getPlutusTx :: PlcCode }

-- | Some sort of transaction fee (we need to determine that more dynamically)
--
standardTxFee :: Value
standardTxFee = 1

data Range a =
    Interval a a -- inclusive-exclusive
    | GEQ a
    | LT a

-- | Event triggers the Plutus client can register with the wallet.
data EventTrigger =
    BlockHeightRange !(Range BlockHeight) -- ^ True when the block height is within the range
    | FundsAtAddress [Address] !(Range Value) -- ^ True when the (unspent) funds at a list of addresses are within the range
    | And EventTrigger EventTrigger -- ^ True when both triggers are true
    | Or EventTrigger EventTrigger -- ^ True when at least one trigger is true
    | PAlways -- ^ Always true
    | PNever -- ^ Never true

-- | Validator scripts expect two scripts and information about the current
--   txn. In the future this will be written in Plutus (with the help of TH)
--   and its return type will be `a` instead of `Maybe a`.
--   See https://github.com/input-output-hk/plutus-prototype/tree/master/docs/extended-utxo#extension-to-validator-scripts
--
type Validator = PlutusTx

type Redeemer = PlutusTx

type DataScript = PlutusTx

{- Note [Transaction Templates]

Transaction templates are currently missing from this mock API and will be
added in the future.

Transaction templates differ from transactions in at least two ways:

1) They do not include a transaction fee (that is, the sum of their input
   values equals the sum of their output values)
2) Part of their input value is not attributed to an address

To turn a template into a transaction, the wallet
1) Adjusts either the input values or the output value to ensure that the
   difference between inputs and outputs matches the transaction fee.
2) Expands the inputs to account for the missing funds (via coin selection).

These two steps depend on each other because the transaction fee is a
function of the size of the transaction including its
inputs.

-}
