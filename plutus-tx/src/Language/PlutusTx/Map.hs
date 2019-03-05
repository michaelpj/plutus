{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Map (
    Map,
    Comparison,
    keys,
    toList,
    size,
    valid,
    nil,
    nilOrd,
    insert,
    delete,
    lookup,
    union)
where

import           Prelude hiding (lookup)
import qualified Language.PlutusTx.Map.TH as TH
import           Language.PlutusTx.Map.TH (Map(..), BST(..), Comparison)

valid :: Map k v -> Bool
valid = $$(TH.valid)

keys :: Map k v -> [k]
keys = $$(TH.keys)

toList :: Map k v -> [(k, v)]
toList = $$(TH.toList)

size :: Map k v -> Int
size = $$(TH.size)

-- | Make an empty map, using the comparison function from the 'Ord' instance.
nilOrd :: (Ord k) => Map k v
nilOrd = Map compare Leaf

-- | Make an empty map.
nil :: Comparison k -> Map k v
nil = $$(TH.nil)

-- | Insert a key into a map.
insert :: k -> v -> Map k v -> Map k v
insert = $$(TH.insert)

-- | Delete a key from a map.
delete :: k -> Map k v -> Map k v
delete = $$(TH.delete)

-- | Lookup a value in a map
lookup :: k -> Map k v -> Maybe v
lookup = $$(TH.lookup)

-- | Union two maps together.
union :: Map k v -> Map k v -> Map k v
union = $$(TH.union)
