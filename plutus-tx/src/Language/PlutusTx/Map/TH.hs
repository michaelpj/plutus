{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.PlutusTx.Map.TH (
    Map(..),
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

import Prelude hiding (lookup, all, length)
import           Language.Haskell.TH          (Q, TExp)
import Language.PlutusTx.Prelude (all, length)

type Comparison k = k -> k -> Ordering

-- | A map, implemented as a binary search tree. Unlike @Data.Map@ from @containers@,
-- this does not require an 'Ord' instance for keys, but rather the comparison function
-- is stored in the datastructure. This means we have less efficient 'union', however.
data Map k v = Map (Comparison k) (BST k v)

data BST k v = Leaf | Branch (BST k v) k v (BST k v)

valid :: Q (TExp (Map k v -> Bool))
valid = [|| \(Map comp tree) -> $$(validT) comp tree ||]

keys :: Q (TExp (Map k v -> [k]))
keys = [|| $$(foldMappings) (\(k,_) ks -> k:ks) [] ||]

size :: Q (TExp (Map k v -> Int))
size = [|| \m -> $$(length) ($$(toList) m) ||]

toList :: Q (TExp (Map k v -> [(k, v)]))
toList = [|| $$(foldMappings) (\m ms -> m:ms) [] ||]

nil :: Q (TExp (Comparison k -> Map k v))
nil = [|| \comp -> Map comp $$(nilT) ||]

insert :: Q (TExp (k -> v -> Map k v -> Map k v))
insert = [|| \k v (Map comp tree) -> Map comp ($$(insertT) comp k v tree) ||]

delete :: Q (TExp (k -> Map k v -> Map k v))
delete = [|| \k (Map comp tree) -> Map comp ($$(deleteT) comp k tree) ||]

-- | Lookup a value in a map
lookup :: Q (TExp (k -> Map k v -> Maybe v))
lookup = [|| \k (Map comp tree) -> $$(lookupT) comp k tree ||]

-- | Union two maps together.
union :: Q (TExp (Map k v -> Map k v -> Map k v))
union = [|| \l r -> $$(foldMappings) (\(k, v) acc -> $$(insert) k v acc) r l ||]

foldMappings :: Q (TExp (((k, v) -> b -> b) -> b -> Map k v -> b))
foldMappings = [|| \f acc (Map _ t) -> $$(foldMappingsT) f acc t ||]

validT :: Q (TExp (Comparison k -> BST k v -> Bool))
validT =
    [||
        let
            validT comp = \case
                Leaf -> True
                Branch l k _ r ->
                    validT comp l && validT comp r &&
                    $$(all) (\k' -> comp k' k == LT) ($$(keysT) l) &&
                    $$(all) (\k' -> comp k' k == GT) ($$(keysT) r)
        in validT
    ||]

keysT :: Q (TExp (BST k v -> [k]))
keysT = [|| $$(foldMappingsT) (\(k,_) ks -> k:ks) [] ||]

nilT :: Q (TExp (BST k v))
nilT = [|| Leaf ||]

insertT :: Q (TExp (Comparison k -> k -> v -> BST k v -> BST k v))
insertT =
    [||
        let
            insertT comp k v = \case
                Leaf -> Branch $$(nilT) k v $$(nilT)
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (insertT comp k v l) k' v' r
                    GT -> Branch l k' v' (insertT comp k v r)
                    EQ -> Branch l k' v r
        in insertT
    ||]

deleteT :: Q (TExp (Comparison k -> k -> BST k v -> BST k v))
deleteT =
    [||
        let
            deleteT comp k = \case
                Leaf -> Leaf
                Branch l k' v' r -> case comp k k' of
                    LT -> Branch (deleteT comp k l) k' v' r
                    GT -> Branch l k' v' (deleteT comp k r)
                    EQ -> joinT l r
            joinT :: BST k v -> BST k v -> BST k v
            joinT Leaf r = r
            joinT l Leaf = l
            joinT (Branch l k v r) (Branch l' k' v' r') = Branch l k v (Branch (joinT r l') k' v' r')
        in deleteT
    ||]

lookupT :: Q (TExp (Comparison k -> k -> BST k v -> Maybe v))
lookupT =
    [||
        let
            lookupT comp k = \case
                Leaf -> Nothing
                Branch l k' v r -> case comp k k' of
                    LT -> lookupT comp k l
                    GT -> lookupT comp k r
                    EQ -> Just v
        in lookupT
    ||]

foldMappingsT :: Q (TExp (((k, v) -> b -> b) -> b -> BST k v -> b))
foldMappingsT =
    [||
        let foldMappingsT f acc = \case
                Leaf -> acc
                Branch l k v r ->
                    let
                        right = foldMappingsT f acc r
                        center = f (k, v) right
                        left = foldMappingsT f center l
                    in left
        in foldMappingsT
    ||]
