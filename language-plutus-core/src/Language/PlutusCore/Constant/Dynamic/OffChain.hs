module Language.PlutusCore.Constant.Dynamic.OffChain (
    getStringBuiltinMeanings, getStringBuiltinTypes ) where

import           Control.Monad.Except
import           Language.PlutusCore.Constant.Dynamic.BuiltinName
import           Language.PlutusCore.Constant.Function
import           Language.PlutusCore.Constant.Typed
import           Language.PlutusCore.Error
import           Language.PlutusCore.Quote
import           Language.PlutusCore.TypeCheck

getStringBuiltinMeanings
    :: DynamicBuiltinNameMeanings
getStringBuiltinMeanings =
       insertDynamicBuiltinNameDefinition dynamicTraceDefinitionMock $
       insertDynamicBuiltinNameDefinition dynamicCharToStringDefinition $
       insertDynamicBuiltinNameDefinition dynamicAppendDefinition mempty

getStringBuiltinTypes
    :: (AsTypeError e ann, MonadError e m, MonadQuote m) => ann -> m DynamicBuiltinNameTypes
getStringBuiltinTypes ann = dynamicBuiltinNameMeaningsToTypes ann getStringBuiltinMeanings
