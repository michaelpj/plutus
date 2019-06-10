{-# LANGUAGE OverloadedStrings #-}
module TransformSpec where

import           Common
import           TestLib

import           Language.PlutusCore.Quote

import           Language.PlutusIR.Parser
import           Language.PlutusIR.Transform.NonStrict
import           Language.PlutusIR.Transform.ThunkRecursions

transform :: TestNested
transform = testNested "transform" [
    thunkRecursions
    , nonStrict
    ]

thunkRecursions :: TestNested
thunkRecursions = testNested "thunkRecursions"
    $ map (goldenPir thunkRecursions term)
    [ "listFold"
    , "monoMap"
    ]

nonStrict :: TestNested
nonStrict = testNested "nonStrict"
    $ map (goldenPir (runQuote . compileNonStrictBindings) term)
    [ "nonStrict1"
    ]
