{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Map.Spec (tests) where

import Prelude hiding (lookup)
import Hedgehog
import Language.PlutusTx.Map
import Test.Tasty.Hedgehog
import Test.Tasty
import Control.Applicative
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Property as Prop
import qualified Data.List as L

showMap :: (Show k, Show v) => Map k v -> String
showMap = show . toList

genMap :: (Ord k) => Gen k -> Gen v -> Gen (Map k v)
genMap genKey genValue = Gen.recursive
    Gen.choice
    [pure nilOrd]
    [insert <$> genKey <*> genValue <*> genMap genKey genValue]

genIntKey :: Gen Int
genIntKey = Gen.integral (Range.linear 0 100)

genIntValue :: Gen Int
genIntValue = Gen.integral (Range.linear 0 100)

genIntMap :: Gen (Map Int Int)
genIntMap = genMap genIntKey genIntValue

prop_GenValid :: Property
prop_GenValid = property $ do
    t <- forAllWith showMap genIntMap
    assert (valid t)

prop_NilValid :: Property
prop_NilValid = property $ assert (valid (nilOrd @Int @Int))

prop_InsertValid :: Property
prop_InsertValid = property $ do
    k <- forAll genIntKey
    v <- forAll genIntValue
    t <- forAllWith showMap genIntMap
    assert $ valid (insert k v t)

prop_DeleteValid :: Property
prop_DeleteValid = property $ do
    k <- forAll genIntKey
    t <- forAllWith showMap genIntMap
    assert $ valid (delete k t)

prop_UnionValid :: Property
prop_UnionValid = property $ do
    t1 <- forAllWith showMap genIntMap
    t2 <- forAllWith showMap genIntMap
    assert $ valid (t1 `union` t2)

prop_NilPost :: Property
prop_NilPost = property $ do
    k <- forAll genIntKey
    lookup k (nilOrd @Int @Int) === Nothing

prop_InsertPost :: Property
prop_InsertPost = property $ do
  k <- forAll genIntKey
  k' <- forAll genIntKey
  v <- forAll genIntValue
  t <- forAllWith showMap genIntMap
  lookup k' (insert k v t) === if k==k' then Just v else lookup k' t

prop_DeletePost :: Property
prop_DeletePost = property $ do
  k <- forAll genIntKey
  k' <- forAll genIntKey
  t <- forAllWith showMap genIntMap
  lookup k' (delete k t) === if k==k' then Nothing else lookup k' t

prop_LookupPostPresent :: Property
prop_LookupPostPresent = property $ do
  k <- forAll genIntKey
  v <- forAll genIntValue
  t <- forAllWith showMap genIntMap
  lookup k (insert k v t) === Just v

prop_LookupPostAbsent :: Property
prop_LookupPostAbsent = property $ do
  k <- forAll genIntKey
  t <- forAllWith showMap genIntMap
  lookup k (delete k t) === Nothing

prop_UnionPost :: Property
prop_UnionPost = property $ do
  k <- forAll genIntKey
  t1 <- forAllWith showMap genIntMap
  t2 <- forAllWith showMap genIntMap
  lookup k (t1 `union` t2) === (lookup k t1 <|> lookup k t2)

prop_SizeNil :: Property
prop_SizeNil = property $ size (nilOrd @Int @Int) === 0

prop_SizeInsert :: Property
prop_SizeInsert = property $ do
  k <- forAll genIntKey
  v <- forAll genIntValue
  t <- forAllWith showMap genIntMap
  assert $ size (insert k v t) >= size t

prop_SizeDelete :: Property
prop_SizeDelete = property $ do
  k <- forAll genIntKey
  t <- forAllWith showMap genIntMap
  assert $ size (delete k t) <= size t

prop_SizeUnion :: Property
prop_SizeUnion = property $ do
  t1 <- forAllWith showMap genIntMap
  t2 <- forAllWith showMap genIntMap
  assert $ size (t1 `union` t2) <= size t1 + size t2

deleteKey :: Eq k => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/=k).fst)

prop_NilModel :: Property
prop_NilModel = property $ toList (nilOrd @Int @Int) === []

prop_InsertModel :: Property
prop_InsertModel = property $ do
  k <- forAll genIntKey
  v <- forAll genIntValue
  t <- forAllWith showMap genIntMap
  toList (insert k v t) === L.insert (k,v) (deleteKey k $ toList t)

prop_DeleteModel :: Property
prop_DeleteModel = property $ do
  k <- forAll genIntKey
  t <- forAllWith showMap genIntMap
  toList (delete k t) === deleteKey k (toList t)

prop_LookupModel :: Property
prop_LookupModel = property $ do
  k <- forAll genIntKey
  t <- forAllWith showMap genIntMap
  lookup k t === L.lookup k (toList t)

prop_UnionModel :: Property
prop_UnionModel = property $ do
  t1 <- forAllWith showMap genIntMap
  t2 <- forAllWith showMap genIntMap
  toList (union t1 t2) === L.sort (toList t1 ++ foldr deleteKey (toList t2) (keys t1))

tests :: TestTree
tests =
    let g = $$(discover)
    -- TODO: upstream this
    in testGroup
       (Prop.unGroupName $ groupName g)
       (fmap (\(n, p) -> testProperty (Prop.unPropertyName n) p) (groupProperties g))
