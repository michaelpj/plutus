{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simulating laziness.
module Language.Plutus.CoreToPLC.Laziness where

import Language.Plutus.CoreToPLC.Compiler.Types
import {-# SOURCE #-} Language.Plutus.CoreToPLC.Compiler.Type
import {-# SOURCE #-} Language.Plutus.CoreToPLC.Compiler.Expr

import qualified Language.PlutusCore                  as PLC
import           Language.PlutusCore.Quote

import qualified GhcPlugins                                  as GHC

{- Note [Object- vs meta-language combinators]
Many of the things we define as *meta*-langugage combinators (i.e. operations on terms) could be defined
as combinators in the object language (i.e. terms). For example, we can define 'delay' as taking a term
and returning a lambda that takes unit and returns the term, or we could define a 'delay' term

\t : a . \u : unit . t

We generally prefer the metalanguage approach despite the fact that we could share combinators
with the standard library because it makes the generated terms simpler without the need for
a simplifier pass. Also, PLC isn't lazy, so combinators work less well.
-}

delay :: Converting m => PLC.Term PLC.TyName PLC.Name () -> m (PLC.Term PLC.TyName PLC.Name ())
delay body = PLC.LamAbs () <$> liftQuote (freshName () "thunk") <*> convType GHC.unitTy <*> pure body

delayType :: Converting m => PLC.Type PLC.TyName () -> m (PLC.Type PLC.TyName ())
delayType orig = PLC.TyFun () <$> convType GHC.unitTy <*> pure orig

force :: Converting m => PLC.Term PLC.TyName PLC.Name () -> m (PLC.Term PLC.TyName PLC.Name ())
force thunk = PLC.Apply () thunk <$> convExpr (GHC.Var GHC.unitDataConId)

maybeDelay :: Converting m => Bool -> PLC.Term PLC.TyName PLC.Name () -> m (PLC.Term PLC.TyName PLC.Name ())
maybeDelay yes t = if yes then delay t else pure t

maybeDelayType :: Converting m => Bool -> PLC.Type PLC.TyName () -> m (PLC.Type PLC.TyName ())
maybeDelayType yes t = if yes then delayType t else pure t

maybeForce :: Converting m => Bool -> PLC.Term PLC.TyName PLC.Name () -> m (PLC.Term PLC.TyName PLC.Name ())
maybeForce yes t = if yes then force t else pure t
