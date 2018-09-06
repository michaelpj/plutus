{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS -fplugin=Language.Plutus.CoreToPLC.Plugin #-}
-- the simplfiier messes with things otherwise
{-# OPTIONS_GHC   -O0 #-}

module Main (main) where


import           Test.Tasty
import           Test.Tasty.Golden

import qualified Data.ByteString.Lazy                                as BSL
import           Data.Text.Encoding                                  (encodeUtf8)

import           Language.Plutus.Coordination.Contracts.CrowdFunding
import           Language.Plutus.Coordination.Plutus
import           Language.Plutus.CoreToPLC.Plugin                    (PlcCode, getAst)
import           Language.PlutusCore                                 (debugText)

main :: IO ()
main = defaultMain tests

golden :: String -> PlcCode -> TestTree
golden name value = (goldenVsString name ("test/" ++ name ++ ".plc.golden") . pure . BSL.fromStrict . encodeUtf8 . debugText . getAst) value

tests :: TestTree
tests = testGroup "Validator scripts" [
    golden "crowdfunding" crowdfunding
  ]

crowdfunding :: PlcCode
crowdfunding = getPlutusTx $ contributionScript c Nothing where
  c = Campaign {
    campaignDeadline = 100,
    campaignTarget   = 10,
    campaignCollectionDeadline = 120,
    campaignOwner = PubKey
  }
