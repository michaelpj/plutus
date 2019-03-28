module Ledger.Blockchain where

import           Control.Monad   (join)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (listToMaybe)
import qualified Data.Set        as Set

import           Ledger.Crypto
import           Ledger.Interval (Slot (..))
import           Ledger.Scripts
import           Ledger.Tx
import           Ledger.Value    (Value)

-- | A block on the blockchain. This is just a list of transactions which
-- successfully validate following on from the chain so far.
type Block = [Tx]
-- | A blockchain, which is just a list of blocks, starting with the newest.
type Blockchain = [Block]

-- | The slot number of the last slot of a blockchain. Assumes that each slot
--   has precisely one block. This is true in the
--   emulator but not necessarily on the real chain.
lastSlot :: Blockchain -> Slot
lastSlot = Slot . length

-- | Lookup a transaction in a 'Blockchain' by its id.
transaction :: Blockchain -> TxId -> Maybe Tx
transaction bc txid = listToMaybe $ filter p  $ join bc where
    p = (txid ==) . hashTx

-- | Determine the unspent output that an input refers to
out :: Blockchain -> TxOutRef -> Maybe TxOut
out bc o = do
    t <- transaction bc (txOutRefId o)
    let i = txOutRefIdx o
    if length (txOutputs t) <= i
        then Nothing
        else Just $ txOutputs t !! i

-- | Determine the unspent value that a transaction output refers to.
value :: Blockchain -> TxOutRef -> Maybe Value
value bc o = txOutValue <$> out bc o

-- | Determine the data script that a transaction output refers to.
dataTxo :: Blockchain -> TxOutRef -> Maybe DataScript
dataTxo bc o = txOutData =<< out bc o

-- | Determine the public key that locks a transaction output, if there is one.
pubKeyTxo :: Blockchain -> TxOutRef -> Maybe PubKey
pubKeyTxo bc o = out bc o >>= txOutPubKey

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRefOf (hashTx t) idx, o)

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> Set.Set TxOutRef
spentOutputs = Set.map txInRef . txInputs

-- | The unspent transaction outputs of the ledger as a whole.
unspentOutputs :: Blockchain -> Map TxOutRef TxOut
unspentOutputs = foldr updateUtxo Map.empty . join

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo t unspent = (unspent `Map.difference` lift' (spentOutputs t)) `Map.union` outs where
    lift' = Map.fromSet (const ())
    outs = unspentOutputsTx t
