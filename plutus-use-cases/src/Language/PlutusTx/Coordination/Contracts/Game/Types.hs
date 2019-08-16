{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Coordination.Contracts.Game.Types where

import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude

newtype HashedString = HashedString ByteString

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString

PlutusTx.makeLift ''ClearString
