{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusCore.Evaluation.Stats where

import           Control.Lens

import qualified Data.Map                  as Map
import           Data.String
import           Data.Text.Prettyprint.Doc

data Stats = Stats {
      _typeInstantiations   :: Integer
    , _functionApplications :: Integer
    , _unwraps              :: Integer
    , _builtinEvaluations   :: Map.Map String Integer
} deriving (Eq, Ord, Show)

makeLenses ''Stats

instance Semigroup Stats where
    c1 <> c2 = Stats
        (c1 ^. typeInstantiations + c2 ^. typeInstantiations)
        (c1 ^. functionApplications + c2 ^. functionApplications)
        (c1 ^. unwraps + c2 ^. unwraps)
        (c1 ^. builtinEvaluations <> c2 ^. builtinEvaluations)

instance Monoid Stats where
    mempty = Stats 0 0 0 mempty

instance Pretty Stats where
    pretty c = fromString $ show c

addTypeInstantiation :: Stats -> Stats
addTypeInstantiation c = c & typeInstantiations +~ 1

addFunctionApplication :: Stats -> Stats
addFunctionApplication c = c & functionApplications +~ 1

addUnwrap :: Stats -> Stats
addUnwrap c = c & unwraps +~ 1

addBuiltinEvaluation :: String -> Stats -> Stats
addBuiltinEvaluation name c = c & builtinEvaluations . at name %~ \case
    Nothing -> Just 1
    Just times -> Just $ times + 1
