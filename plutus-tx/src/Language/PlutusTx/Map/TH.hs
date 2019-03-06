{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.PlutusTx.Map.TH (
    Map,
    RBTree(..),
    Comparison,
    nil,
    singleton,
    lookup,
    keys,
    values,
    toList,
    size,
    map,
    foldr,
    valid,
    insert,
    --delete,
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
type Map = RBTree

data Color = B | R
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

type BlackHeight = Int

data RBTree k v = Leaf | Branch Color BlackHeight (RBTree k v) k v (RBTree k v)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

makeLift ''Color
makeLift ''RBTree

------------------------------------------------------------
-- Straightforward functions
------------------------------------------------------------

nil :: Q (TExp (RBTree k v))
nil = [|| Leaf ||]

singleton :: Q (TExp (k -> v -> RBTree k v))
singleton = [|| \k v -> Branch B 1 Leaf k v Leaf ||]

height :: Q (TExp (RBTree k v -> BlackHeight))
height = [|| \case { Leaf -> 0 ; Branch _ h _ _ _ _ -> h } ||]

lookup :: Q (TExp (Comparison k -> k -> RBTree k v -> Maybe v))
lookup =
    [|| \comp k ->
        let go = \case
                Leaf -> Nothing
                Branch _ _ l k' v r -> case comp k k' of
                    LT -> go l
                    GT -> go r
                    EQ -> Just v
        in go
    ||]

size :: Q (TExp (RBTree k v -> Int))
size = [|| $$length . $$keys ||]

keys :: Q (TExp (RBTree k v -> [k]))
keys = [|| $$foldr (\(k,_) ks -> k:ks) [] ||]

values :: Q (TExp (RBTree k v -> [v]))
values = [|| $$foldr (\(_,v) vs -> v:vs) [] ||]

toList :: Q (TExp (RBTree k v -> [(k, v)]))
toList = [|| $$(foldr) (\m ms -> m:ms) [] ||]

map :: Q (TExp ((a -> b) -> RBTree k a -> RBTree k b))
map =
    [||
        let map f = \case
                Leaf -> Leaf
                Branch c h l k v r -> Branch c h (map f l) k (f v) (map f r)
        in map
    ||]

foldr :: Q (TExp (((k, v) -> b -> b) -> b -> RBTree k v -> b))
foldr =
    [||
        let foldr f acc = \case
                Leaf -> acc
                Branch _ _ l k v r ->
                    let
                        right = foldr f acc r
                        center = f (k, v) right
                        left = foldr f center l
                    in left
        in foldr
    ||]

------------------------------------------------------------
-- Invariants and validity
------------------------------------------------------------

isBalanced :: Q (TExp (RBTree k v -> Bool))
isBalanced = [|| \t -> $$isBlackSame t && $$isRedSeparate t ||]

isBlackSame :: Q (TExp (RBTree k v -> Bool))
isBlackSame =
    [||
        \t -> case $$blacks t of
              [] -> True
              n:ns -> $$all (n==) ns
    ||]

blacks :: Q (TExp (RBTree k v -> [Int]))
blacks =
    [||
         let blacks' n = \case
                Leaf -> [n+1]
                Branch R _ l _ _ r -> blacks' n  l ++ blacks' n  r
                Branch B _ l _ _ r -> blacks' (n+1) l ++ blacks' (n+1) r
         in blacks' 0
    ||]

isRedSeparate :: Q (TExp (RBTree k v -> Bool))
isRedSeparate = [|| $$reds B ||]

reds :: Q (TExp (Color -> RBTree k v -> Bool))
reds =
    [||
        let
            reds _ Leaf = True
            reds R (Branch R _ _ _ _ _) = False
            reds _ (Branch c _ l _ _ r) = reds c l && reds c r
        in reds
    ||]

isOrdered :: Q (TExp (Comparison k -> RBTree k v -> Bool))
isOrdered =
    [|| \comp t ->
            let
                ordered [] = True
                ordered [_] = True
                ordered (x:y:xys) = comp x y == LT && ordered (y:xys)
            in ordered $ $$keys t
    ||]

correctBlackHeight :: Q (TExp (RBTree k v -> Bool))
correctBlackHeight =
    [|| \t ->
            let
                correct n Leaf = n == 0
                correct n (Branch R h l _ _ r) = n == h' && correct n l && correct n r
                  where h' = h - 1
                correct n (Branch B h l _ _ r) = n == h && correct n' l && correct n' r
                  where n' = n - 1
            in correct ($$height t) t
    ||]

valid :: Q (TExp (Comparison k -> RBTree k v -> Bool))
valid = [|| \comp t -> $$isBalanced t && $$correctBlackHeight t && $$isOrdered comp t ||]

------------------------------------------------------------
-- Colour switching
------------------------------------------------------------

turnR :: Q (TExp (RBTree k v -> RBTree k v))
turnR = [|| \case { Leaf -> Leaf; Branch _ h l k v r -> Branch R h l k v r } ||]

turnB :: Q (TExp (RBTree k v -> RBTree k v))
turnB = [|| \case { Leaf -> Leaf; Branch _ h l k v r -> Branch B h l k v r } ||]

------------------------------------------------------------
-- Insertion and balancing
------------------------------------------------------------

insert :: Q (TExp (Comparison k -> k -> v -> RBTree k v -> RBTree k v))
insert =
    [|| \comp k v t ->
        let go = \case
                Leaf -> Branch R 1 Leaf k v Leaf
                (Branch B h l k' v' r) -> case comp k k' of
                    LT -> $$balanceL (Branch B h (go l) k' v' r)
                    GT -> $$balanceR (Branch B h l k' v' (go r))
                    EQ -> Branch B h l k' v r
                (Branch R h l k' v' r) -> case comp k k' of
                    LT -> Branch R h (go l) k' v' r
                    GT -> Branch R h l k' v' (go r)
                    EQ -> Branch R h l k' v r
        in $$turnB (go t)
    ||]

balanceL :: Q (TExp (RBTree k v -> RBTree k v))
balanceL =
    [|| \t -> case t of
            Branch B h toSplit k3 v3 t4 -> case toSplit of
                Branch R _ (Branch R _ t1 k1 v1 t2) k2 v2 t3 ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                Branch R _ t1 k1 v1 (Branch R _ t2 k2 v2 t3) ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                _ -> t
            _ -> t
    ||]

balanceR :: Q (TExp (RBTree k v-> RBTree k v))
balanceR =
    [|| \t -> case t of
            Branch B h t1 k1 v1 toSplit -> case toSplit of
                Branch R _ t2 k2 v2 (Branch R _ t3 k3 v3 t4) ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                Branch R _ (Branch R _ t2 k2 v2 t3) k3 v3 t4 ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                _ -> t
            _ -> t
    ||]

------------------------------------------------------------
-- Joining and splitting
------------------------------------------------------------

cmpInt :: Q (TExp (Int -> Int -> Ordering))
cmpInt = [|| \i1 i2 -> if i1<i2 then LT else if i1 == i2 then EQ else GT ||]

-- | Join two trees, with an optional key-value mapping in the middle. The keys in the
-- left tree are assumed to be less than the key in the middle, which is less than the
-- keys in the right tree.
join :: Q (TExp (Comparison k -> k -> v -> RBTree k v -> RBTree k v -> RBTree k v))
join = [|| \comp k v ->
    let join Leaf t2 = $$insert comp k v t2
        join t1 Leaf = $$insert comp k v t1
        join t1 t2 = case $$cmpInt h1 h2 of
            LT -> $$turnB $ joinLT t1 t2 h1
            GT -> $$turnB $ joinGT t1 t2 h2
            EQ -> Branch B (h1+1) t1 k v t2
          where
            h1 = $$height t1
            h2 = $$height t2
        joinLT t1 t2@(Branch c h l k' v' r) h1
          | h == h1   = Branch R (h+1) t1 k v t2
          | otherwise = $$balanceL (Branch c h (joinLT t1 l h1) k' v' r)
        joinLT t1 Leaf _ = t1
        joinGT t1@(Branch c h l k' v' r) t2 h2
          | h == h2   = Branch R (h+1) t1 k v t2
          | otherwise = $$balanceR (Branch c h l k' v' (joinGT r t2 h2))
        joinGT Leaf t2 _ = t2
    in join
    ||]

-- | Given a key @k@, spits a tree into a left tree with keys less than @k@, optionally a value
-- corresponding to the mapping for @k@ if there is one, and a right tree with keys greater
-- than @k@.
split :: Q (TExp (Comparison k -> k -> RBTree k v -> (RBTree k v, Maybe v, RBTree k v)))
split =
    [|| \comp needle ->
        let go = \case
                Leaf -> (Leaf, Nothing, Leaf)
                Branch _ _ l k v r -> case comp needle k of
                    LT -> let (l', mid, r') = go l in (l', mid, $$join comp k v r' ($$turnB r))
                    GT -> let (l', mid, r') = go r in ($$join comp k v ($$turnB l) l', mid, r')
                    EQ -> ($$turnB l, Just v, $$turnB r)
        in go
    ||]

------------------------------------------------------------
-- Union
------------------------------------------------------------

-- | Union two trees, keeping both values for a key in case it appears on both sides.
-- This is the most general union function, all the others can be derived by mapping over
-- the resulting structure.
unionThese :: Q (TExp (Comparison k -> RBTree k a -> RBTree k b -> RBTree k (These a b)))
unionThese =
    [|| \comp ->
        let union l Leaf = $$turnB $ $$map This l
            union Leaf r = $$turnB $ $$map That r
            union (Branch _ _ l k v r) t =
                -- There are several ways to do a union of RBTree. This way has the
                -- key advantage of pulling out the mapping for k if there is one,
                -- so we can package it up in a 'These' properly.
                let (l', v', r') = $$split comp k t
                    values = $$andMaybe v v'
                in $$join comp k values (union l l') (union r r')
        in union
    ||]

-- | Union two trees, using the given function to compute a value when a key has a
-- mapping on both sides.
unionWith :: Q (TExp (Comparison k -> (a -> a -> a) -> RBTree k a -> RBTree k a -> RBTree k a))
unionWith = [|| \comp with l r -> $$map ($$mergeThese with) ($$unionThese comp l r) ||]

-- | Left-biased union of two trees.
union :: Q (TExp (Comparison k -> RBTree k a -> RBTree k a -> RBTree k a))
union = [|| \comp -> $$(unionWith) comp (\a _ -> a) ||]

------------------------------------------------------------

{-
delete :: Q (TExp (Comparison k -> k -> RBTree k v -> RBTree k v))
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
-}
