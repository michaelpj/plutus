{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions for compiling PIR let terms.
module Language.PlutusIR.Compiler.Case (compileCases) where

import           Language.PlutusIR
import           Language.PlutusIR.Compiler.Datatype
import           Language.PlutusIR.Compiler.Error
import           Language.PlutusIR.Compiler.Provenance
import           Language.PlutusIR.Compiler.Recursion
import           Language.PlutusIR.Compiler.Types
import qualified Language.PlutusIR.MkPir               as PIR

import           Control.Monad
import           Control.Monad.Error.Lens

import           Control.Lens                          hiding (Strict)

import           Data.List


compileCases :: Compiling m e a => PIRTerm a -> m (PIRTerm a)
compileCases = transformMOf termSubterms compileCase

compileCases :: Compiling m e a => PIRTerm a -> m (PIRTerm a)
compileCases = \case
    Case p s m ty bs -> withEnclosing (const $ CaseExpression p) $ case s of
            Strict -> mkIterApp p (TyInst p m ty) bs
            NonStrict -> do
                -- TODO: These are created at every use site, we should bind them globally
                argName <- liftQuote $ freshName ann "arg"
                let unit = ann <$ Unit.unit
                    unitval = ann <$ Unit.unitval
                    delay t = Apply ann t unitval
                    force t = Apply ann t unitval
                force (mkIterApp p (TyInst p m (TyFun p unit ty) (force <$> bs)))
    x -> pure x
