{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
-- | Contract interface for the guessing game
module Main where

import           Control.Lens                                  (at, (^.))
import qualified Data.Aeson                                    as Aeson
import qualified Data.Map                                      as Map
import           Data.Maybe                                    (fromMaybe)
import           GHC.Generics                                  (Generic)
import           Network.Wai.Handler.Warp                      (run)

import           Language.Plutus.Contract                      (PlutusContract, emit, endpoint, watchAddress)
import           Language.Plutus.Contract.Servant              (contractApp)
import           Language.Plutus.Contract.Transaction          (unbalancedTx)
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress, gameDataScript, gameRedeemerScript,
                                                                gameValidator)

import qualified Ledger                                        as L
import           Ledger.Ada                                    (Ada)
import qualified Ledger.Ada                                    as Ada
import qualified Ledger.AddressMap                             as AM

main :: IO ()
main = run 8080 (contractApp game)

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: String
    , amount     :: Ada
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: String
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

-- | State
newtype GameState = GameState
    { interestingAddresses :: AM.AddressMap
    }
    deriving stock (Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

initialState :: GameState
initialState = GameState mempty

guess :: PlutusContract ()
guess = do
  st <- watchAddress gameAddress
  let mp = AM.fromTxOutputs st
  GuessParams theGuess <- endpoint "guess"
  let
    outputs  = fmap fst . Map.toList . fromMaybe Map.empty $ mp ^. at gameAddress
    redeemer = gameRedeemerScript theGuess
    inp      = (\o -> L.scriptTxIn o gameValidator redeemer) <$> outputs
    tx       = unbalancedTx inp []
  emit tx

lock :: PlutusContract ()
lock = do
  LockParams secret amt <- endpoint "lock"
  let
    vl         = Ada.toValue amt
    dataScript = gameDataScript secret
    output = L.TxOutOf gameAddress vl (L.PayToScript dataScript)
    tx     = unbalancedTx [] [output]
  emit tx

game :: PlutusContract ()
game = guess <> lock
