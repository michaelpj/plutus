{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeApplications #-}
module Language.Plutus.Contract.Effects.ExposeEndpoint where

import           Data.Proxy
import           Data.Row
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           GHC.TypeLits                     (symbolVal)

import           Language.Plutus.Contract.Request as Req

newtype EndpointDescription = EndpointDescription String
    deriving (Eq, Ord)

type EndpointReq s = s .== Set EndpointDescription
type EndpointResp s a = s .== a

-- | Expose an endpoint, return the data that was entered
endpoint :: forall s a. (KnownSymbol s) => Contract (EndpointResp s a) (EndpointReq s) a
endpoint = mkRequest s Just where
  s = Set.singleton $ EndpointDescription $ symbolVal (Proxy @s)

