{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- | This module defines a red-black tree suitable for use as a map or set.
--
-- The code is heavily inspired by the @llrbtree@ Haskell library, as well
-- as @https://github.com/sweirich/dth/blob/master/examples/red-black/RedBlack.lhs@.
-- However, this version has been modified significantly for TH usage and clarity.
--
-- This implementation also aims to be relatively elementary, since we don't have
-- features like GADTs which some implementations use to improve safety.
--
-- As a reminder, red-black trees have a number of invariants (some of the things that
-- are normally enforced as invariants are here ensured by the types):
-- (1) The root is black.
-- (2) If a node is red, then both its children are black.
-- (3) Every path from a given node to any of its leaves has the same number of
--     black nodes.
-- Invariant (2) and (3) between them ensure that the tree is balanced.
module Language.PlutusTx.RBTree (
    RBTree(..),
    Color(..),
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
import           Language.PlutusTx.Prelude    (all, length, intCompare)
import           Language.PlutusTx.These
import           Language.PlutusTx.Lift
import           Codec.Serialise
import           GHC.Generics

type Comparison k = k -> k -> Ordering

data Color = B | R
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

-- | The number of black nodes between this node and the leaves. Invariant (3) states
-- that this is the same for all paths, so can be represented with a single value.
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

blackHeight :: Q (TExp (RBTree k v -> BlackHeight))
blackHeight = [|| \case { Leaf -> 0 ; Branch _ h _ _ _ _ -> h } ||]

color :: Q (TExp (RBTree k v -> Color))
color = [|| \case { Leaf -> B ; Branch c _ _ _ _ _ -> c } ||]

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
size = [|| \t -> $$length $ $$keys t ||]

keys :: Q (TExp (RBTree k v -> [k]))
keys = [|| $$foldr (\(k,_) ks -> k:ks) [] ||]

values :: Q (TExp (RBTree k v -> [v]))
values = [|| $$foldr (\(_,v) vs -> v:vs) [] ||]

toList :: Q (TExp (RBTree k v -> [(k, v)]))
toList = [|| $$foldr (\m ms -> m:ms) [] ||]

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
isBalanced = [|| \t -> $$sameBlacksToLeaves t && $$checkChildren t ||]

sameBlacksToLeaves :: Q (TExp (RBTree k v -> Bool))
sameBlacksToLeaves =
    [|| \t -> case $$blacksToLeaves t of
              [] -> True
              n:ns -> $$all (n==) ns
    ||]

blacksToLeaves :: Q (TExp (RBTree k v -> [Int]))
blacksToLeaves =
    [||
         let blacksToLeavesFrom :: Int -> RBTree k v -> [Int]
             blacksToLeavesFrom n = \case
                Leaf -> [n+1]
                Branch R _ l _ _ r -> blacksToLeavesFrom n l ++ blacksToLeavesFrom n r
                Branch B _ l _ _ r -> blacksToLeavesFrom (n+1) l ++ blacksToLeavesFrom (n+1) r
         in blacksToLeavesFrom 0
    ||]

checkChildren :: Q (TExp (RBTree k v -> Bool))
checkChildren =
    [|| \t ->
        let
            checkVsParentColor _ Leaf = True
            -- Red child of red parent - invariant violation!
            checkVsParentColor R (Branch R _ _ _ _ _) = False
            checkVsParentColor _ (Branch c _ l _ _ r) = checkVsParentColor c l && checkVsParentColor c r
        in checkVsParentColor ($$color t) t
    ||]

keysSorted :: Q (TExp (Comparison k -> RBTree k v -> Bool))
keysSorted =
    [|| \comp t ->
            let sorted [] = True
                sorted [_] = True
                sorted (x:y:xys) = comp x y == LT && sorted (y:xys)
            in sorted $ $$keys t
    ||]

correctBlackHeight :: Q (TExp (RBTree k v -> Bool))
correctBlackHeight =
    [|| \t ->
            let correct n Leaf = n == 0
                correct n (Branch R h l _ _ r) = n == h' && correct n l && correct n r
                  where h' = h - 1
                correct n (Branch B h l _ _ r) = n == h && correct n' l && correct n' r
                  where n' = n - 1
            in correct ($$blackHeight t) t
    ||]

valid :: Q (TExp (Comparison k -> RBTree k v -> Bool))
valid = [|| \comp t -> $$isBalanced t && $$correctBlackHeight t && $$keysSorted comp t ||]

------------------------------------------------------------
-- Colour switching
------------------------------------------------------------

redden :: Q (TExp (RBTree k v -> RBTree k v))
redden = [|| \case { Leaf -> Leaf; Branch _ h l k v r -> Branch R h l k v r } ||]

blacken :: Q (TExp (RBTree k v -> RBTree k v))
blacken = [|| \case { Leaf -> Leaf; Branch _ h l k v r -> Branch B h l k v r } ||]

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
        in $$blacken (go t)
    ||]

{- Note [Balancing]
There are four cases for (locally) balancing a subtree, each of which
produces the same top-level pattern, visible on the right-hand sides
of each of the cases below.

The cases split into those that balance the left side, and those that
balance the right side. Often we know that only one side can be unbalanced,
so it is advantageous to run only one side.

L1:
          B3           R2
         /  \         /  \
        R2   T  ->   B1  B3
       /  \         / \  / \
      R1   T        T T  T T
     /  \
    T    T

L2:
        B3             R2
       /  \           /  \
      R1   T    ->   B1  B3
     /  \           / \  / \
    T    R2         T T  T T
        /  \
       T    T

R1:
      B1               R2
     /  \             /  \
    T   R2      ->   B1  B3
       /  \         / \  / \
      T   R1        T T  T T
         /  \
        T    T

R2:
      B1               R2
     /  \             /  \
    T   R2      ->   B1  B3
       /  \         / \  / \
      R1   T        T T  T T
     /  \
    T    T
-}

-- | Correct a local imbalance on the left side of the tree.
balanceL :: Q (TExp (RBTree k v -> RBTree k v))
balanceL =
    [|| \t -> case t of
            Branch B h toSplit k3 v3 t4 -> case toSplit of
                -- See note [Balancing], this is case L1
                Branch R _ (Branch R _ t1 k1 v1 t2) k2 v2 t3 ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                -- See note [Balancing], this is case L2
                Branch R _ t1 k1 v1 (Branch R _ t2 k2 v2 t3) ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                _ -> t
            _ -> t
    ||]

-- | Correct a local imbalance on the right side of the tree.
balanceR :: Q (TExp (RBTree k v -> RBTree k v))
balanceR =
    [|| \t -> case t of
            Branch B h t1 k1 v1 toSplit -> case toSplit of
                -- See note [Balancing], this is case R1
                Branch R _ t2 k2 v2 (Branch R _ t3 k3 v3 t4) ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                -- See note [Balancing], this is case R2
                Branch R _ (Branch R _ t2 k2 v2 t3) k3 v3 t4 ->
                    Branch R (h+1) (Branch B h t1 k1 v1 t2) k2 v2 (Branch B h t3 k3 v3 t4)
                _ -> t
            _ -> t
    ||]

------------------------------------------------------------
-- Joining and splitting
------------------------------------------------------------

-- | Join two trees, with a key-value mapping in the middle. The keys in the
-- left tree are assumed to be less than the key in the middle, which is less than the
-- keys in the right tree.
join :: Q (TExp (Comparison k -> k -> v -> RBTree k v -> RBTree k v -> RBTree k v))
join = [|| \comp k v ->
    let bh = $$blackHeight
        ins = $$insert comp k v

        join Leaf t2 = ins t2
        join t1 Leaf = ins t1
        join t1 t2 = case $$intCompare h1 h2 of
            LT -> $$blacken $ joinLT t1 t2 h1
            GT -> $$blacken $ joinGT t1 t2 h2
            EQ -> $$blacken $ joinEQ t1 t2 h1
          where
            h1 = bh t1
            h2 = bh t2

        joinLT t1 t2@(Branch c h l k' v' r) h1
          | h == h1   = joinEQ t1 t2 h
          | otherwise = $$balanceL (Branch c h (joinLT t1 l h1) k' v' r)
        joinLT t1 Leaf _ = t1

        joinGT t1@(Branch c h l k' v' r) t2 h2
          | h == h2   = joinEQ t1 t2 h
          | otherwise = $$balanceR (Branch c h l k' v' (joinGT r t2 h2))
        joinGT Leaf t2 _ = t2

        joinEQ t1 t2 h = Branch R (h+1) t1 k v t2
    in join
    ||]

-- | Given a key @k@, spits a tree into a left tree with keys less than @k@, optionally a value
-- corresponding to the mapping for @k@ if there is one, and a right tree with keys greater
-- than @k@.
split :: Q (TExp (Comparison k -> k -> RBTree k v -> (RBTree k v, Maybe v, RBTree k v)))
split =
    [|| \comp needle ->
        let jn = $$join comp
            go = \case
                Leaf -> (Leaf, Nothing, Leaf)
                Branch _ _ l k v r -> case comp needle k of
                    LT -> let (l', mid, r') = go l in (l', mid, jn k v r' ($$blacken r))
                    GT -> let (l', mid, r') = go r in (jn k v ($$blacken l) l', mid, r')
                    EQ -> ($$blacken l, Just v, $$blacken r)
        in go
    ||]

------------------------------------------------------------
-- Union
------------------------------------------------------------

-- | Union two trees, keeping both values for a key in case it appears on both sides.
-- This is the most general union function, but it is less efficient than 'unionWith'
-- and 'union'.
unionThese :: Q (TExp (Comparison k -> RBTree k a -> RBTree k b -> RBTree k (These a b)))
unionThese =
    [|| \comp ->
        -- The cases for leaves meant that this is less efficient than a simple union:
        -- it must look at every element (this is obvious from the type!).
        let union l Leaf = $$blacken $ $$map This l
            union Leaf r = $$blacken $ $$map That r
            union (Branch _ _ l k v r) t =
                -- There are several ways to do a union of RBTree. This way has the
                -- key advantage of pulling out the mapping for k if there is one,
                -- so we can package it up in a 'These' properly.
                let (l', maybeV, r') = $$split comp k t
                    theseVs = $$andMaybe v maybeV
                in $$join comp k theseVs (union l l') (union r r')
        in union
    ||]

-- | Union two trees, using the given function to compute a value when a key has a
-- mapping on both sides.
unionWith :: Q (TExp (Comparison k -> (a -> a -> a) -> RBTree k a -> RBTree k a -> RBTree k a))
unionWith =
    [|| \comp with->
        let union l Leaf = $$blacken $ l
            union Leaf r = $$blacken $ r
            union (Branch _ _ l k v r) t =
                let (l', maybeV, r') = $$split comp k t
                    newV = case maybeV of
                        Just v' -> with v v'
                        Nothing -> v
                in $$join comp k newV (union l l') (union r r')
        in union
    ||]

-- | Left-biased union of two trees.
union :: Q (TExp (Comparison k -> RBTree k a -> RBTree k a -> RBTree k a))
union = [|| \comp -> $$(unionWith) comp (\a _ -> a) ||]

------------------------------------------------------------
-- Deletion
------------------------------------------------------------

{-
delete :: Q (TExp (Comparison k -> k -> RBTree k v -> RBTree k v))
delete = [|| \comp k t -> let (l, _, r) =  $$split comp k t in $$join comp l r ||]
-}
