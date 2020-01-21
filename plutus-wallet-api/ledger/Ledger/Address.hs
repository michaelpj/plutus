{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
module Ledger.Address (
    Address (..),
    pubKeyAddress,
    scriptAddress,
    scriptHashAddress,
    ) where

import qualified Codec.CBOR.Write          as Write
import           Codec.Serialise.Class     (Serialise, encode)
import           Crypto.Hash               (Digest, SHA256, hash)
import           Data.Aeson                (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import qualified Data.ByteArray            as BA
import qualified Data.ByteString.Lazy      as BSL
import           Data.Hashable             (Hashable, hashWithSalt)
import           Data.String               (IsString (..))
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           IOTS                      (IotsType)

import           Ledger.Crypto
import           Ledger.Orphans            ()
import           Ledger.Scripts
import           LedgerBytes               (LedgerBytes (..))

-- | A payment address using a hash as the id.
data Address = PubKeyAddress PubKeyHash
    | ScriptAddress ValidatorHash
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, IotsType, Serialise)

instance Pretty Address where
    pretty (PubKeyAddress pkh) = pretty pkh
    pretty (ScriptAddress vh)  = pretty vh

-- TODO: might need something for the alternative
instance Hashable Address where
    hashWithSalt s (PubKeyAddress pkh) = hashWithSalt s pkh
    hashWithSalt s (ScriptAddress vh)  = hashWithSalt s vh

-- | The address that should be targeted by a transaction output locked by the given public key.
pubKeyAddress :: PubKey -> Address
pubKeyAddress pk = PubKeyAddress $ pubKeyHash pk

-- | The address that should be used by a transaction output locked by the given validator script.
scriptAddress :: Validator -> Address
scriptAddress = ScriptAddress . validatorHash

-- | The address that should be used by a transaction output locked by the given validator script.
scriptHashAddress :: ValidatorHash -> Address
scriptHashAddress = ScriptAddress
