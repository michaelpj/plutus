{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.Servant(
      contractServer
    , contractApp
    , initialResponse
    , runUpdate
    , Request(..)
    , Response(..)
    ) where

import           Control.Monad.Except               (MonadError (..), runExcept)
import           Control.Monad.Writer
import           Data.Aeson                         (FromJSON(..))
import qualified Data.Aeson                         as Aeson
import           Data.Bifunctor
import           Data.Proxy                         (Proxy (..))
import           Data.Row
import           Data.String                        (IsString (fromString))
import           GHC.Generics                       (Generic)
import           Servant                            ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody, err500, errBody)
import           Servant.Server                     (Application, ServantErr, Server, serve)

import           Language.Plutus.Contract
import           Language.Plutus.Contract.Record
import           Language.Plutus.Contract.Request   (Event, Hooks, applyEndo)
import           Language.Plutus.Contract.Resumable (ResumableError)
import qualified Language.Plutus.Contract.Resumable as Resumable
import           Language.Plutus.Contract.Schema    (JsonRow (..))

newtype State e = State { record :: Record e }
    deriving stock (Generic)

instance (Forall ρ FromJSON, AllUniqueLabels ρ) => FromJSON (State (Event ρ)) where
    parseJSON v = fmap (State . fmap (fmap unJsonRow)) (parseJSON v)

instance Forall ρ Eq => Eq (State (Event ρ))

data Request ρ = Request
    { oldState :: State (Event ρ)
    , event    :: Event ρ
    }
    deriving stock (Generic)

data Response ρ σ = Response
    { newState :: State (Event ρ)
    , hooks    :: Hooks σ
    }

instance (Forall σ Eq, Forall ρ Eq) => Eq (Response ρ σ)

type ContractAPI ρ σ =
       "initialise" :> Get '[JSON] (Response ρ σ)
  :<|> "run" :> ReqBody '[JSON] (Request ρ) :> Post '[JSON] (Response ρ σ)

-- | Serve a 'PlutusContract' via the contract API.
contractServer 
    :: forall ρ σ. 
       ( AllUniqueLabels ρ
       , Forall σ Monoid )
    => Contract ρ σ ()
    -> Server (ContractAPI ρ σ)
contractServer con = initialise :<|> run where
    initialise = servantResp (initialResponse con)
    run req = servantResp (runUpdate con req)

servantResp :: MonadError ServantErr m => Either ResumableError (Response ρ σ) -> m (Response ρ σ)
servantResp = \case
        Left err ->
            let bd = "'insertAndUpdate' failed. " in
            throwError $ err500 { errBody = fromString (bd <> show err) }
        Right r -> pure r

-- | A servant 'Application' that serves a Plutus contract
contractApp 
    :: forall ρ σ. 
       ( AllUniqueLabels ρ
       , Forall σ Monoid )
    => Contract ρ σ () -> Application
contractApp = serve (Proxy @(ContractAPI ρ σ)) . contractServer

runUpdate 
    :: forall ρ σ.
       ( AllUniqueLabels ρ
       , Forall σ Monoid)
    => Contract ρ σ () -> Request ρ -> Either ResumableError (Response ρ σ)
runUpdate con (Request o e) =
    (\(r, h) -> Response (State r) h)
    <$> Resumable.insertAndUpdate (Resumable.mapStep (Resumable.mapO applyEndo) con) (record o) e

initialResponse :: Contract ρ σ () -> Either ResumableError (Response ρ σ)
initialResponse =
    second (uncurry Response . first (State . fmap fst))
    . runExcept
    . runWriterT
    . Resumable.initialise
