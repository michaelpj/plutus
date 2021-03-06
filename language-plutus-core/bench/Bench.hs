{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Language.PlutusCore
import qualified Language.PlutusCore.Check.Normal           as Normal
import           Language.PlutusCore.Constant.Dynamic
import           Language.PlutusCore.Evaluation.Machine.Cek (unsafeEvaluateCek)
import           Language.PlutusCore.Evaluation.Machine.Ck  (unsafeEvaluateCk)
import           Language.PlutusCore.Pretty

import           Codec.Serialise
import           Control.Monad
import           Criterion.Main
import           Crypto
import qualified Data.ByteString.Lazy                       as BSL

pubKey, sig, msg :: BSL.ByteString
sig = "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"
pubKey = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
msg = ""

traceBuiltins :: QuoteT (Either (Error ())) DynamicBuiltinNameTypes
traceBuiltins = getStringBuiltinTypes ()

main :: IO ()
main =
    defaultMain [ env largeTypeFiles $ \ ~(f, g, h) ->
                    let mkBench = bench "pretty" . nf (fmap prettyPlcDefText) . parse
                    in

                    bgroup "prettyprint" $ mkBench <$> [f, g, h]

                , env typeCompare $ \ ~(f, g) ->
                  let parsed0 = parse f
                      parsed1 = parse g
                  in
                    bgroup "equality"
                        [ bench "Program equality" $ nf ((==) <$> parsed0 <*>) parsed1
                        ]

                , env files $ \ ~(f, g) ->
                    bgroup "parse"
                      [ bench "addInteger" $ nf parse f
                      , bench "stringLiteral" $ nf parse g
                      ]

                , env sampleScript $ \ f ->
                  let typeCheckConcrete :: Program TyName Name () -> Either (Error ()) (Normalized (Type TyName ()))
                      typeCheckConcrete p = runQuoteT $ do
                            bis <- traceBuiltins
                            inferTypeOfProgram (defOffChainConfig { _tccDynamicBuiltinNameTypes = bis }) p
                      mkBench = bench "type-check" . nf typeCheckConcrete . deserialise
                  in

                  bgroup "type-check" $ mkBench <$> [f]

                , env largeTypeFiles $ \ ~(f, g, h) ->
                  let typeCheckConcrete :: Program TyName Name AlexPosn -> Either (Error AlexPosn) (Normalized (Type TyName ()))
                      typeCheckConcrete = runQuoteT . inferTypeOfProgram defOffChainConfig
                      mkBench = bench "type-check" . nf (typeCheckConcrete =<<) . runQuoteT . parseScoped
                  in

                   bgroup "type-check" $ mkBench <$> [f, g, h]
                , env largeTypeFiles $ \ ~(f, g, h) ->
                   let normalConcrete :: Program TyName Name AlexPosn -> Either (Error AlexPosn) ()
                       normalConcrete = Normal.checkProgram
                       mkBench = bench "normal-types check" . nf (normalConcrete =<<) . runQuoteT . parseScoped
                   in
                   bgroup "normal-types check" $ mkBench <$> [f, g, h]

                , env sampleScript $ \ f ->
                    let renameConcrete :: Program TyName Name () -> Program TyName Name ()
                        renameConcrete = runQuote . rename
                        mkBench = bench "rename (Plutus Tx)" . nf renameConcrete . deserialise
                  in

                  bgroup "renamer" $ mkBench <$> [f]

                , env largeTypeFiles $ \ ~(f, g, h) ->
                    let renameConcrete :: Program TyName Name AlexPosn -> Program TyName Name AlexPosn
                        renameConcrete = runQuote . rename
                        mkBench = bench "rename" . nf (fmap renameConcrete) . parse
                    in

                    bgroup "renamer" $ mkBench <$> [f, g, h]

                , env largeTypeFiles $ \ ~(f, g, h) ->
                    let mkBench src = bench "serialise" $ nf (fmap (serialise . void)) $ parse src
                    in

                    bgroup "CBOR" $ mkBench <$> [f, g, h]

                , env largeTypeFiles $ \ ~(f, g, h) ->
                    let deserialiseProgram :: BSL.ByteString -> Program TyName Name ()
                        deserialiseProgram = deserialise
                        parseAndSerialise :: BSL.ByteString -> Either (ParseError AlexPosn) BSL.ByteString
                        parseAndSerialise = fmap (serialise . void) . parse
                        mkBench src = bench "deserialise" $ nf (fmap deserialiseProgram) $ parseAndSerialise src
                    in

                    bgroup "CBOR" $ mkBench <$> [f, g, h]

                , env evalFiles $ \ ~(f, g) ->
                    let processor :: BSL.ByteString -> Either (Error AlexPosn) (Program TyName Name ())
                        processor contents = void <$> (runQuoteT $ parseScoped contents)
                        f' = processor f
                        g' = processor g
                    in

                    bgroup "unsafeEvaluateCk"
                      [ bench "valid" $ nf (fmap $ unsafeEvaluateCk . toTerm) f'
                      , bench "invalid" $ nf (fmap $ unsafeEvaluateCk . toTerm) g'
                      ]
                , env evalFiles $ \ ~(f, g) ->
                   let processor :: BSL.ByteString -> Either (Error AlexPosn) (Program TyName Name ())
                       processor contents = void <$> (runQuoteT $ parseScoped contents)
                       f' = processor f
                       g' = processor g
                   in

                   bgroup "unsafeEvaluateCek"
                     [ bench "valid" $ nf (fmap $ unsafeEvaluateCek mempty . toTerm) f'
                     , bench "invalid" $ nf (fmap $ unsafeEvaluateCek mempty . toTerm) g'
                     ]
                ,   bgroup "verifySignature" $
                      let verify :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString -> Maybe Bool
                          verify = verifySignature
                      in

                      [ bench "valid" $ nf (verify pubKey msg) sig
                      , bench "invalid" $ nf (verify msg pubKey) sig
                      ]

                ]

    where envFile = BSL.readFile "test/data/addInteger.plc"
          stringFile = BSL.readFile "test/data/stringLiteral.plc"
          files = (,) <$> envFile <*> stringFile
          largeTypeFile0 = BSL.readFile "test/types/negation.plc"
          largeTypeFile1 = BSL.readFile "test/types/tail.plc"
          largeTypeFile2 = BSL.readFile "test/types/verifyIdentity.plc"
          largeTypeFiles = (,,) <$> largeTypeFile0 <*> largeTypeFile1 <*> largeTypeFile2
          typeCompare0 = BSL.readFile "test/types/example.plc"
          typeCompare1 = BSL.readFile "bench/example-compare.plc"
          typeCompare = (,) <$> typeCompare0 <*> typeCompare1
          evalFile0 = BSL.readFile "test/Evaluation/Golden/verifySignature.plc"
          evalFile1 = BSL.readFile "test/Evaluation/Golden/verifySignatureError.plc"
          evalFiles = (,) <$> evalFile0 <*> evalFile1
          sampleScript = BSL.readFile "bench/script.plci"
