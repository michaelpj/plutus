{-# LANGUAGE FlexibleContexts #-}
module Language.PlutusCore
    (
      -- * Parser
      parse
    , parseST
    , parseTermST
    , parseTypeST
    , parseScoped
    , parseProgram
    , parseTerm
    , parseType
    -- * Pretty-printing
    , Configuration (..)
    , defaultCfg
    , debugCfg
    , prettyCfgText
    , prettyCfgString
    , debugText
    , prettyString
    -- * AST
    , Term (..)
    , Type (..)
    , Constant (..)
    , Kind (..)
    , ParseError (..)
    , Version (..)
    , Program (..)
    , Name (..)
    , TyName (..)
    , Unique (..)
    , Size
    , Value
    , BuiltinName (..)
    , TypeBuiltin (..)
    -- * Lexer
    , AlexPosn (..)
    -- * Views
    , IterApp (..)
    , TermIterApp
    , PrimIterApp
    -- * Formatting
    , format
    , formatDoc
    -- * Processing
    , annotateProgram
    , annotateTerm
    , annotateType
    , RenameError (..)
    , TyNameWithKind (..)
    , NameWithType (..)
    , TypeState (..)
    , RenamedType
    , RenamedTerm
    -- * Normalization
    , check
    , checkProgram
    , checkTerm
    , NormalizationError
    , checkFile
    -- * Type synthesis
    , typecheckProgram
    , typecheckTerm
    , kindCheck
    , fileType
    , fileTypeCfg
    , printType
    , TypeError (..)
    , TypeCheckM
    , BuiltinTable (..)
    , parseTypecheck
    -- * Serialization
    , encodeProgram
    , decodeProgram
    , readProgram
    , writeProgram
    -- * Errors
    , PrettyCfg (..)
    , Error (..)
    , IsError (..)
    -- * Base functors
    , TermF (..)
    , TypeF (..)
    -- * Quotation and term construction
    , Quote
    , runQuote
    , QuoteT
    , runQuoteT
    , MonadQuote
    , liftQuote
    , convertErrors
    -- * Name generation
    , freshUnique
    , freshName
    , freshTyName
    -- * Quasi-Quoters
    , plcType
    , plcTerm
    , plcProgram
    -- * Evaluation
    , parseRunCk
    , CkEvalResult (..)
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import           Data.Text.Prettyprint.Doc
import           Language.PlutusCore.CBOR
import           Language.PlutusCore.CkMachine
import           Language.PlutusCore.Error
import           Language.PlutusCore.Lexer
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.Name
import           Language.PlutusCore.Normalize
import           Language.PlutusCore.Parser
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Quote
import           Language.PlutusCore.Renamer
import           Language.PlutusCore.TH
import           Language.PlutusCore.Type
import           Language.PlutusCore.TypeSynthesis
import           Language.PlutusCore.View
import           PlutusPrelude

-- | Given a file at @fibonacci.plc@, @fileType "fibonacci.plc"@ will display
-- its type or an error message.
fileType :: FilePath -> IO T.Text
fileType = fmap (either prettyCfgText id . printType) . BSL.readFile

-- | Given a file, display
-- its type or an error message, optionally dumping annotations and debug
-- information.
fileTypeCfg :: Configuration -> FilePath -> IO T.Text
fileTypeCfg cfg = fmap (either (renderCfg cfg) id . printType) . BSL.readFile

checkFile :: FilePath -> IO (Maybe T.Text)
checkFile bs = do
    input <- BSL.readFile bs
    let result = runExcept $ runQuoteT (checkProgram =<< parseProgram input)
    pure $ case result of
        Left e -> Just (prettyCfgText e)
        Right _ -> Nothing

-- | Print the type of a program contained in a 'ByteString'
printType :: (MonadError (Error AlexPosn) m) => BSL.ByteString -> m T.Text
printType bs = runQuoteT $ prettyCfgText <$> (typecheckProgram 1000 <=< annotateProgram <=< checkProgram <=< (liftEither . convertError . parseScoped)) bs

-- | Parse and rewrite so that names are globally unique, not just unique within
-- their scope.
parseScoped :: (MonadError (Error AlexPosn) m) => BSL.ByteString -> m (Program Type TyName Name AlexPosn)
parseScoped str = liftEither $ convertError $ fmap (\(p, s) -> rename s p) $ runExcept $ runStateT (parseST str) emptyIdentifierState

-- | Parse a program and typecheck it.
parseTypecheck :: (MonadError (Error AlexPosn) m, MonadQuote m) => Natural -> BSL.ByteString -> m (NormalizedType TyNameWithKind ())
parseTypecheck gas = typecheckProgram gas <=< annotateProgram <=< checkProgram <=< parseProgram

-- | Parse a program and run it using the CK machine.
parseRunCk :: (MonadError (Error AlexPosn) m) => BSL.ByteString -> m CkEvalResult
parseRunCk = fmap (runCk . void) . parseScoped

formatDoc :: (MonadError (Error AlexPosn) m) => BSL.ByteString -> m (Doc a)
formatDoc bs = runQuoteT $ prettyCfg defaultCfg <$> parseProgram bs

format :: (MonadError (Error AlexPosn) m) => Configuration -> BSL.ByteString -> m T.Text
format cfg = fmap (render . prettyCfg cfg) . parseScoped
