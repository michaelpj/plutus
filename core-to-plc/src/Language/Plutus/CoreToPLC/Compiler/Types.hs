{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}

module Language.Plutus.CoreToPLC.Compiler.Types where

import           Language.Plutus.CoreToPLC.Error
import           Language.Plutus.CoreToPLC.PLCTypes

import qualified Language.PlutusCore.MkPlc          as PLC
import           Language.PlutusCore.Quote

import qualified GhcPlugins                         as GHC

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.List.NonEmpty                 as NE
import qualified Data.Map                           as Map
import           Lens.Micro

import qualified Language.Haskell.TH.Syntax         as TH

type BuiltinNameInfo = Map.Map TH.Name GHC.TyThing

newtype ConversionOptions = ConversionOptions { coCheckValueRestriction :: Bool }

data ConvertingContext = ConvertingContext {
    ccOpts            :: ConversionOptions,
    ccFlags           :: GHC.DynFlags,
    ccBuiltinNameInfo :: BuiltinNameInfo,
    ccScopes          :: ScopeStack
    }

-- | The visibility of a definition. See Note [Abstract data types]
data Visibility = Abstract | Visible
-- | A definition of type 'val' with variable type 'var'.
data Def var val = Def {dVis::Visibility, dVar::var, dVal::val}

-- | Either a simple type or a datatype with constructors and a matcher.
data TypeRep = PlainType PLCType | DataType PLCType [TermDef] TermDef

type TypeDef = Def PLCTyVar TypeRep

instance Show (Def PLCTyVar TypeRep) where
    show Def{dVar=v} = show (PLC.tyVarDeclName v)

type TermDef = Def PLCVar PLCTerm

instance Show (Def PLCVar PLCTerm) where
    show Def{dVar=v} = show (PLC.varDeclName v)

type DefMap key def = Map.Map key (def, [key])

data ConvertingState = ConvertingState {
    csTypeDefs :: DefMap GHC.Name TypeDef,
    csTermDefs :: DefMap GHC.Name TermDef
    }

typeDefs :: Lens' ConvertingState (DefMap GHC.Name TypeDef)
typeDefs = lens g s where
    g = csTypeDefs
    s cs tds = cs { csTypeDefs = tds }

termDefs :: Lens' ConvertingState (DefMap GHC.Name TermDef)
termDefs = lens g s where
    g = csTermDefs
    s cs tds = cs { csTermDefs = tds }

-- See Note [Scopes]
type Converting m = (Monad m, MonadError ConvError m, MonadQuote m, MonadReader ConvertingContext m, MonadState ConvertingState m)

runConverting :: ConvertingContext -> ConvertingState -> ReaderT ConvertingContext (StateT ConvertingState (QuoteT (Except ConvError))) a -> Either ConvError a
runConverting context initialState = runExcept . runQuoteT . flip evalStateT initialState . flip runReaderT context

{- Note [Scopes]
We need a notion of scope, because we have to make sure that if we convert a GHC
Var into a variable, then we always convert it into the same variable, while also making
sure that if we encounter multiple things with the same name we produce fresh variables
appropriately.

So we have the usual mechanism of carrying around a stack of scopes.
-}

data Scope = Scope (Map.Map GHC.Name PLCVar) (Map.Map GHC.Name PLCTyVar)
type ScopeStack = NE.NonEmpty Scope

initialScopeStack :: ScopeStack
initialScopeStack = pure $ Scope Map.empty Map.empty
