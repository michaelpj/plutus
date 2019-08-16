{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-- | Run a Plutus contract as a servant application.
module Language.Plutus.Contract.App(
      run
    , runWithTraces
    , Wallet(..)
    ) where

import           Control.Monad                    (foldM_)
import qualified Data.Aeson                       as Aeson
import           Data.Aeson                       (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy.Char8       as BSL
import           Data.Foldable                    (traverse_)
import qualified Data.Map                         as Map
import           Data.Row
import           Language.Plutus.Contract
import           Language.Plutus.Contract.Servant (Request (..), Response (..), contractApp, initialResponse, runUpdate)
import           Language.Plutus.Contract.Trace   (ContractTrace, EmulatorAction, execTrace)
import qualified Network.Wai.Handler.Warp         as Warp
import           System.Environment               (getArgs)
import           Wallet.Emulator                  (Wallet (..))

import Language.Plutus.Contract.Rows.Instances    ()

-- | Run the contract as an HTTP server with servant/warp
run 
    :: forall ρ σ. 
       ( AllUniqueLabels ρ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ ToJSON
       , Forall ρ FromJSON
       , Forall ρ ToJSON )
    => Contract ρ σ () -> IO ()
run st = runWithTraces st []

-- | Run the contract as an HTTP server with servant/warp, and
--   print the 'Request' values for the given traces.
runWithTraces
    :: forall ρ σ. 
       ( AllUniqueLabels ρ
       , AllUniqueLabels σ
       , Forall σ Monoid
       , Forall σ ToJSON
       , Forall ρ FromJSON
       , Forall ρ ToJSON )
    => Contract ρ σ ()
    -> [(String, (Wallet, ContractTrace ρ σ EmulatorAction () ()))]
    -> IO ()
runWithTraces con traces = do
    let mp = Map.fromList traces
    args <- getArgs
    case args of
        [] -> do
            let p = 8080
            putStrLn $ "Starting server on port " ++ show p
            Warp.run p (contractApp con)
        ["trace", t] -> maybe (printTracesAndExit mp) (uncurry (printTrace con)) (Map.lookup t mp)
        _ -> printTracesAndExit mp

-- | Print a list of available traces
printTracesAndExit :: Map.Map String a -> IO ()
printTracesAndExit mp = do
    putStrLn "list of available traces (call with 'trace ${trace}')"
    traverse_ putStrLn (Map.keysSet mp)

-- | Run a trace on the mockchain and print the 'Request' JSON objects
--   for each intermediate state to stdout.
printTrace 
    :: forall ρ σ. 
       ( AllUniqueLabels σ
       , Forall σ Monoid
       , Forall ρ ToJSON )
    => Contract ρ σ () 
    -> Wallet 
    -> ContractTrace ρ σ EmulatorAction () () 
    -> IO ()
printTrace con wllt ctr = do
    let events = Map.findWithDefault [] wllt $ execTrace con ctr
        go previous evt = do
            let st = newState previous
                newRequest = Request { oldState = st, event = evt }
            BSL.putStrLn (Aeson.encode newRequest)
            either (error . show) pure (runUpdate con newRequest)

    initial <- either (error . show) pure (initialResponse con)
    foldM_ go initial events

