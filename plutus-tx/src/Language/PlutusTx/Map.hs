{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Map (
    Map,
    Comparison,
    keys,
    toList,
    size,
    valid,
    nil,
    insert,
    delete,
    lookup,
    union,
    unionThese)
where

import           Prelude hiding (lookup)
import qualified Language.PlutusTx.Map.TH as TH
import           Language.PlutusTx.Map.TH (Map, Comparison)
import           Language.PlutusTx.These

valid :: Comparison k -> Map k v -> Bool
valid = $$(TH.valid)

keys :: Map k v -> [k]
keys = $$(TH.keys)

toList :: Map k v -> [(k, v)]
toList = $$(TH.toList)

size :: Map k v -> Int
size = $$(TH.size)

    -- | Make an empty map.
nil :: Map k v
nil = $$(TH.nil)

-- | Insert a key into a map.
insert :: Comparison k -> k -> v -> Map k v -> Map k v
insert = $$(TH.insert)

-- | Delete a key from a map.
delete :: Comparison k -> k -> Map k v -> Map k v
delete = $$(TH.delete)

-- | Lookup a value in a map
lookup :: Comparison k -> k -> Map k v -> Maybe v
lookup = $$(TH.lookup)

-- | Union two maps together, keeping the mapping on the left if a key appears in both sides.
union :: Comparison k -> Map k a -> Map k a -> Map k a
union = $$(TH.union)

-- | Union two maps together, keeping both values when there is mapping in both sides.
unionThese :: Comparison k -> Map k a -> Map k b -> Map k (These a b)
unionThese = $$(TH.unionThese)
