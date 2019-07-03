{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module SchemaSpec where

import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Schema       (Constructor (Constructor, Record), ConstructorName (ConstructorName),
                               DataType (DataType), ToSchema, TypeSignature (TypeSignature), argumentSignatures,
                               constructorName, moduleName, toSchema)
import           Test.Hspec   (Spec, describe, it, shouldBe)

spec :: Spec
spec = toSchemaSpec

toSchemaSpec :: Spec
toSchemaSpec =
    describe "To schema" $ do
        it "Int" $
            toSchema (Proxy @Int) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Types"
                     , constructorName = "Int"
                     , argumentSignatures = []
                     })
                []
        it "Integer" $
            toSchema (Proxy @Integer) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Integer.Type"
                     , constructorName = "Integer"
                     , argumentSignatures = []
                     })
                []
        it "String" $
            toSchema (Proxy @String) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Types"
                     , constructorName = "String"
                     , argumentSignatures = []
                     })
                []
        it "Text" $
            toSchema (Proxy @Text) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "Data.Text.Internal"
                     , constructorName = "Text"
                     , argumentSignatures = []
                     })
                []
        it "[Int]" $
            toSchema (Proxy @[Int]) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Types"
                     , constructorName = "[]"
                     , argumentSignatures =
                           [ (TypeSignature
                                  { moduleName = "GHC.Types"
                                  , constructorName = "Int"
                                  , argumentSignatures = []
                                  })
                           ]
                     })
                []
        it "(Int, String)" $
            toSchema (Proxy @(Int, String)) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Tuple"
                     , constructorName = "(,)"
                     , argumentSignatures =
                           [ TypeSignature
                                 { moduleName = "GHC.Types"
                                 , constructorName = "Int"
                                 , argumentSignatures = []
                                 }
                           , TypeSignature
                                 { moduleName = "GHC.Types"
                                 , constructorName = "String"
                                 , argumentSignatures = []
                                 }
                           ]
                     })
                [ Constructor
                      (ConstructorName "Tuple")
                      [ TypeSignature
                            { moduleName = "GHC.Types"
                            , constructorName = "Int"
                            , argumentSignatures = []
                            }
                      , TypeSignature
                            { moduleName = "GHC.Types"
                            , constructorName = "String"
                            , argumentSignatures = []
                            }
                      ]
                ]
        it "Maybe String" $
            toSchema (Proxy @(Maybe String)) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "GHC.Maybe"
                     , constructorName = "Maybe"
                     , argumentSignatures =
                           [ TypeSignature
                                 { moduleName = "GHC.Types"
                                 , constructorName = "String"
                                 , argumentSignatures = []
                                 }
                           ]
                     })
                [ Constructor (ConstructorName "Nothing") []
                , Constructor
                      (ConstructorName "Just")
                      [ TypeSignature
                            { moduleName = "GHC.Types"
                            , constructorName = "String"
                            , argumentSignatures = []
                            }
                      ]
                ]
        it "User" $
            toSchema (Proxy @User) `shouldBe`
            DataType
                (TypeSignature
                     { moduleName = "SchemaSpec"
                     , constructorName = "User"
                     , argumentSignatures = []
                     })
                [ Record
                      (ConstructorName "User")
                      [ ( "userName"
                        , TypeSignature
                              { moduleName = "Data.Text.Internal"
                              , constructorName = "Text"
                              , argumentSignatures = []
                              })
                      , ( "userAge"
                        , TypeSignature
                              { moduleName = "GHC.Types"
                              , constructorName = "Int"
                              , argumentSignatures = []
                              })
                      , ( "userAlive"
                        , TypeSignature
                              { moduleName = "GHC.Types"
                              , constructorName = "Bool"
                              , argumentSignatures = []
                              })
                      ]
                ]

data User =
    User
        { userName  :: Text
        , userAge   :: Int
        , userAlive :: Bool
        }
    deriving (Show, Eq, Generic, ToSchema)
