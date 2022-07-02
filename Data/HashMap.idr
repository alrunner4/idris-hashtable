module Data.HashMap
import Data.HashTable

export
data HashMap k v = MkHashMap (HashTable (k,v))

export newHashMap: Eq k => HasIO m => (k -> Int) -> m (HashMap k v)
export (.insert):  HasIO m => HashMap k v -> k ->       v -> m  Bool
export (.lookup):  HasIO m => HashMap k v -> k ->            m (Maybe v)
export (.delete):  HasIO m => HashMap k v -> k ->            m  Bool

-- TODO
-- export (.replace): HasIO m => HashMap k v -> k -> Maybe v -> m (Maybe v)

newHashMap hash = MkHashMap <$> newHashTable (\(k,_) => hash k)
   @{let eq = (\l,r => fst l == fst r) in MkEq eq (\l,r => not$ eq l r)}

(.insert) (MkHashMap ht) k v = ht.insert (k,v)
(.lookup) (MkHashMap ht) k   = ht.lookup (k, believe_me ()) <&> map snd
(.delete) (MkHashMap ht) k   = ht.delete (k, believe_me ())

