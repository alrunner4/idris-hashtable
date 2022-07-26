module Data.HashMap
import Data.HashTable

export
record HashMap (k: Type) (v: Type) where
   constructor MkHashMap
   hash: k -> Int
   equal: k -> k -> Bool
   table: HashTable (k,v)

export newHashMap:
   {default 32 size: Int} ->
   Eq k => (k -> Int) ->
   HasIO m => m (HashMap k v)

export (.insert): HasIO m => HashMap k v -> k -> v -> m  Bool
export (.lookup): HasIO m => HashMap k v -> k ->      m (Maybe v)
export (.delete): HasIO m => HashMap k v -> k ->      m  Bool

-- TODO
-- export (.replace): HasIO m => HashMap k v -> k -> Maybe v -> m (Maybe v)

equalKey: Eq k => (k -> k -> Bool) -> k -> (k,_) -> Bool
equalKey equal k x = equal k (fst x)

newHashMap hash = MkHashMap hash (==)
   <$> newHashTable (\l,r => fst l == fst r) (\x => hash (fst x)) {size}

(.insert) hm k v = hm.table.insert (k,v)
(.delete) hm k   = deleteByHash (hm.hash k, (\x => hm.equal (fst x) k)) hm.table
(.lookup) hm k   = lookupByHash (hm.hash k, (\x => hm.equal (fst x) k)) hm.table
                   <&> map snd

