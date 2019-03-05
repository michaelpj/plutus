{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.PlutusTx.Map.TH (
    Map,
    BST(..),
    Comparison,
    valid,
    keys,
    toList,
    size,
    foldMappings,
    nil,
    insert,
    delete,
    lookup,
    union)
where

import           Prelude                      hiding (lookup, all, length)
import           Language.Haskell.TH          (Q, TExp)
import           Language.PlutusTx.Prelude    (all, length)
import           Language.PlutusTx.These

type Comparison k = k -> k -> Ordering

-- | A map, implemented as a binary search tree. Unlike @Data.Map@ from @containers@,
-- this does not require an 'Ord' instance for keys, but rather the comparison function
-- must be passed on each usage, with the user required to ensure consistency.
type Map = BST
data BST k v = Leaf | Branch (BST k v) k v (BST k v)

valid :: Q (TExp (Comparison k -> BST k v -> Bool))
valid =
    [||
        let
            valid comp = \case
                Leaf -> True
                Branch l k _ r ->
                    valid comp l && valid comp r &&
                    $$(all) (\k' -> comp k' k == LT) ($$(keys) l) &&
                    $$(all) (\k' -> comp k' k == GT) ($$(keys) r)
        in valid
    ||]

toList :: Q (TExp (BST k v -> [(k, v)]))
toList = [|| $$(foldMappings) (\m ms -> m:ms) [] ||]

keys :: Q (TExp (BST k v -> [k]))
keys = [|| $$(foldMappings) (\(k,_) ks -> k:ks) [] ||]

size :: Q (TExp (BST k v -> Int))
size = [|| $$(length) . $$(keys) ||]

nil :: Q (TExp (BST k v))
nil = [|| Leaf ||]

union :: Q (TExp (Comparison k -> BST k v -> BST k v -> BST k v))
union = [|| \comp t1 t2 -> $$(foldMappings) (\(k, v) t -> $$(insert) comp k v t) t1 t2 ||]

insert :: Q (TExp (Comparison k -> k -> v -> BST k v -> BST k v))
insert =
    [||
        let
            insert comp k v = \case
                Leaf -> Branch $$(nil) k v $$(nil)
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (insert comp k v l) k' v' r
                    GT -> Branch l k' v' (insert comp k v r)
                    EQ -> Branch l k' v r
        in insert
    ||]

delete :: Q (TExp (Comparison k -> k -> BST k v -> BST k v))
delete =
    [||
        let
            delete comp k = \case
                Leaf -> Leaf
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (delete comp k l) k' v' r
                    GT -> Branch l k' v' (delete comp k r)
                    EQ -> join l r
            join :: BST k v -> BST k v -> BST k v
            join Leaf r = r
            join l Leaf = l
            join (Branch l k v r) (Branch l' k' v' r') = Branch l k v (Branch (join r l') k' v' r')
        in delete
    ||]

lookup :: Q (TExp (Comparison k -> k -> BST k v -> Maybe v))
lookup =
    [||
        let
            lookup comp k = \case
                Leaf -> Nothing
                Branch l k' v r -> case comp k k' of
                    LT -> lookup comp k l
                    GT -> lookup comp k r
                    EQ -> Just v
        in lookup
    ||]

foldMappings :: Q (TExp (((k, v) -> b -> b) -> b -> BST k v -> b))
foldMappings =
    [||
        let foldMappings f acc = \case
                Leaf -> acc
                Branch l k v r ->
                    let
                        right = foldMappings f acc r
                        center = f (k, v) right
                        left = foldMappings f center l
                    in left
        in foldMappings
    ||]
