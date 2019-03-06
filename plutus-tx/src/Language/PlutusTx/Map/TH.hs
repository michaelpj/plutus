{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.PlutusTx.Map.TH (
    Map,
    BST(..),
    Comparison,
    valid,
    keys,
    values,
    toList,
    size,
    map,
    foldr,
    nil,
    insert,
    delete,
    lookup,
    union,
    unionWith,
    unionThese)
where

import           Prelude                      hiding (lookup, all, length, map, foldr)
import           Language.Haskell.TH          (Q, TExp)
import           Language.PlutusTx.Prelude    (all, length)
import           Language.PlutusTx.These
import           Language.PlutusTx.Lift
import           Codec.Serialise
import           GHC.Generics

type Comparison k = k -> k -> Ordering

-- | A map, implemented as a binary search tree. Unlike @Data.Map@ from @containers@,
-- this does not require an 'Ord' instance for keys, but rather the comparison function
-- must be passed on each usage, with the user required to ensure consistency.
type Map = BST
data BST k v = Leaf | Branch (BST k v) k v (BST k v)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

makeLift ''BST

valid :: Q (TExp (Comparison k -> BST k v -> Bool))
valid =
    [||
        let
            valid comp = \case
                Leaf -> True
                Branch l k _ r ->
                    valid comp l && valid comp r &&
                    $$all (\k' -> comp k' k == LT) ($$keys l) &&
                    $$all (\k' -> comp k' k == GT) ($$keys r)
        in valid
    ||]

toList :: Q (TExp (BST k v -> [(k, v)]))
toList = [|| $$(foldr) (\m ms -> m:ms) [] ||]

keys :: Q (TExp (BST k v -> [k]))
keys = [|| $$foldr (\(k,_) ks -> k:ks) [] ||]

values :: Q (TExp (BST k v -> [v]))
values = [|| $$foldr (\(_,v) vs -> v:vs) [] ||]

size :: Q (TExp (BST k v -> Int))
size = [|| $$length . $$keys ||]

nil :: Q (TExp (BST k v))
nil = [|| Leaf ||]

-- | Left-biased union of two trees.
union :: Q (TExp (Comparison k -> BST k a -> BST k a -> BST k a))
union = [|| \comp l r -> $$map ($$mergeThese (\a _ -> a)) ($$unionThese comp l r) ||]

-- | Union two trees, using the given function to compute a value when a key has a
-- mapping on both sides.
unionWith :: Q (TExp (Comparison k -> (a -> a -> a) -> BST k a -> BST k a -> BST k a))
unionWith = [|| \comp with l r -> $$map ($$mergeThese with) ($$unionThese comp l r) ||]

-- | Union two trees, keeping both values for a key in case it appears on both sides.
unionThese :: Q (TExp (Comparison k -> BST k a -> BST k b -> BST k (These a b)))
unionThese =
    [|| \comp ->
        let
            union Leaf r = $$map That r
            union l Leaf = $$map This l
            union (Branch l k v r) t =
                -- There are several ways to do a union of BSTs. This way has the
                -- key advantage of pulling out the mapping for k if there is one,
                -- so we can package it up in a 'These' properly.
                let (l', v', r') = $$split comp k t
                    values = $$andMaybe v v'
                in $$join
                   (union l l')
                   (Just (k, values))
                   (union r r')
        in union
    ||]

-- | Given a key @k@, spits a tree into a left tree with keys less than @k@, optionally a value
-- corresponding to the mapping for @k@ if there is one, and a right tree with keys greater
-- than @k@.
split :: Q (TExp (Comparison k -> k -> BST k v -> (BST k v, Maybe v, BST k v)))
split = [|| \comp ->
            let split _ Leaf = (Leaf, Nothing, Leaf)
                split k (Branch l k' v r) = case comp k k' of
                    LT -> let (l', mid, r') = split k l in (l', mid, Branch r' k' v r)
                    EQ -> (l, Just v, r)
                    GT -> let (l', mid, r') = split k r in (Branch l k' v l', mid, r')
            in split
       ||]

-- | Join two trees, with an optional key-value mapping in the middle. The keys in the
-- left tree are assumed to be less than the key in the middle, which is less than the
-- keys in the right tree.
join :: Q (TExp (BST k v -> Maybe (k, v) -> BST k v -> BST k v))
join =
    [|| let join l m r = case m of
                Just (k, v) -> Branch l k v r
                Nothing -> case l of
                    Leaf -> r
                    Branch l' k v r' -> Branch l' k v (join r' Nothing r)
        in join
    ||]

insert :: Q (TExp (Comparison k -> k -> v -> BST k v -> BST k v))
insert =
    [|| \comp k v ->
        let go = \case
                Leaf -> Branch $$nil k v $$nil
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (go l) k' v' r
                    GT -> Branch l k' v' (go r)
                    EQ -> Branch l k' v r
        in go
    ||]

delete :: Q (TExp (Comparison k -> k -> BST k v -> BST k v))
delete =
    [|| \comp k ->
        let go = \case
                Leaf -> Leaf
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (go l) k' v' r
                    GT -> Branch l k' v' (go r)
                    EQ -> $$join l Nothing r
        in go
    ||]

lookup :: Q (TExp (Comparison k -> k -> BST k v -> Maybe v))
lookup =
    [|| \comp k ->
        let go = \case
                Leaf -> Nothing
                Branch l k' v r -> case comp k k' of
                    LT -> go l
                    GT -> go r
                    EQ -> Just v
        in go
    ||]

map :: Q (TExp ((a -> b) -> BST k a -> BST k b))
map =
    [||
        let map f = \case
                Leaf -> Leaf
                Branch l k v r -> Branch (map f l) k (f v) (map f r)
        in map
    ||]

foldr :: Q (TExp (((k, v) -> b -> b) -> b -> BST k v -> b))
foldr =
    [||
        let foldr f acc = \case
                Leaf -> acc
                Branch l k v r ->
                    let
                        right = foldr f acc r
                        center = f (k, v) right
                        left = foldr f center l
                    in left
        in foldr
    ||]
