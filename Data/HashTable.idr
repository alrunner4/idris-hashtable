module Data.HashTable
import Data.List {- catMaybes -}
import Data.Maybe {- isJust -}
import Data.IOArray {- newArray, readArray, writeArray -}
import Data.IORef {- newIORef, modifyIORef, readIORef, writeIORef -}
%default total

-------------
-- Summary --

export data HashTable: Type -> Type
export (.insert): HasIO m => HashTable v -> v -> m  Bool
export (.member): HasIO m => HashTable v -> v -> m  Bool
export (.delete): HasIO m => HashTable v -> v -> m  Bool
export (.lookup): HasIO m => HashTable v -> v -> m (Maybe v)

export newHashTable:
   {default 32 size: Int} ->
   {0 v: Type} -> (v -> v -> Bool) -> (v -> Int) ->
   {0 m: Type -> Type} -> HasIO m =>
   m (HashTable v)

namespace ByEq
   export newHashTable:
      {default 32 size: Int} ->
      {0 v: Type} -> Eq v => (v -> Int) ->
      {0 m: Type -> Type} -> HasIO m =>
      m (HashTable v)

export memberByHash: HasIO m => (Int, v -> Bool) -> HashTable v -> m Bool
export deleteByHash: HasIO m => (Int, v -> Bool) -> HashTable v -> m Bool
export lookupByHash: HasIO m => (Int, v -> Bool) -> HashTable v -> m (Maybe v)

export toList: HashTable v -> HasIO m => m (List v)
export debug: Show v => HashTable v -> IO ()

------------
-- Detail --

data Probe v = Present v | Deleted

export
record HashTable (v: Type) where
   constructor HashArray
   hash:  v -> Int
   equal: v -> v -> Bool
   load:  IORef Int
   array: IORef (IOArray (Probe v))

Show v => Show (Probe v) where
   show (Present v) = "Present " ++ show v
   show Deleted = "Deleted"

debug ht = do
   l <- readIORef ht.load
   a <- readIORef ht.array
   probe_count <- newIORef 0
   putStr "TABLE DEBUG: BEGIN\n"
   for_ [0 .. max a - 1]$ \i => do
      x <- readArray a i
      whenJust x (\_ => modifyIORef probe_count (+1))
      printLn x
   let probe_weight = cast {to=Double} !(readIORef probe_count) / cast (max a) * 100
   putStr "load: \{show l} of \{show$ max a}\n"
   putStr "probe weight: \{substr 0 4$ show$ probe_weight}%\n"
   putStr "TABLE DEBUG: END\n"

newHashTable equal hash = do
   array <- newArray size
   HashArray hash equal <$> newIORef 0 <*> newIORef array

namespace ByEq
   newHashTable hash = newHashTable {size} (==) hash

loadThreshold: Double
loadThreshold = 0.8

grow: HashTable v -> HasIO m => m ()
grow ht = do
   array <- readIORef ht.array
   currentLoad <- readIORef ht.load
   writeIORef ht.load 0
   grownArray <- newArray ((currentLoad+1)*3)
   writeIORef ht.array grownArray
   for_ [0 .. max array - 1]$ \i => do
      Just (Present e) <- readArray array i
         | _ => pure ()
      ignore$ assert_total$ ht.insert e

(.lookup) ht x = lookupByHash (ht.hash x, ht.equal x) ht
(.member) ht x = memberByHash (ht.hash x, ht.equal x) ht
(.delete) ht x = deleteByHash (ht.hash x, ht.equal x) ht

(.insert) ht x = do
   array <- readIORef ht.array
   currentLoad <- readIORef ht.load
   let loadFactor = cast (currentLoad+1) / cast (max array)
   let hashed = ht.hash x
   when (loadFactor > loadThreshold) (grow ht)
   array <- readIORef ht.array
   let probe: Int -> m Bool
       probe probe_offset = do
         let array_pos = (hashed + probe_offset) `mod` max array
         case !(readArray array array_pos) of
            Nothing => do
               ignore$ writeArray array array_pos (Present x)
               modifyIORef ht.load (+1)
               pure True
            Just (Present e) => if ht.equal x e
                                then pure False
                                else assert_total$ probe (probe_offset+1)
            Just  Deleted    =>      assert_total$ probe (probe_offset+1)
   probe 0

lookupByHash (hashed, selecting) ht = let
   probe: Int -> m (Maybe v)
   probe probe_offset = do
      array <- readIORef ht.array
      let array_pos = (hashed + probe_offset) `mod` max array
      if probe_offset >= max array
         then pure Nothing
         else case !(readArray array array_pos) of
                 Just (Present e) => if selecting e
                                     then pure (Just e)
                                     else assert_total$ probe (probe_offset+1)
                 Just  Deleted   =>       assert_total$ probe (probe_offset+1)
                 Nothing         => pure Nothing
   in probe 0

memberByHash (hashed, selecting) ht = isJust <$> lookupByHash (hashed, selecting) ht

deleteByHash (hashed, selecting) ht = let
   probe: Int -> m Bool
   probe probe_offset = do
      array <- readIORef ht.array
      let array_pos = (hashed + probe_offset) `mod` max array
      if probe_offset >= max array
         then pure False
         else case !(readArray array array_pos) of
                 Just (Present e) => if   selecting e
                                     then do
                                          True <- writeArray array array_pos Deleted
                                             | False => pure False
                                          modifyIORef ht.load (\l => l-1)
                                          pure True
                                     else assert_total$ probe (probe_offset+1)
                 Just  Deleted    =>      assert_total$ probe (probe_offset+1)
                 Nothing          =>   pure False
   in probe 0

toList ht = do
   array <- readIORef ht.array
   for [0 .. max array - 1] (readArray array)
      <&> mapMaybe (\case
            Nothing          => Nothing
            Just  Deleted    => Nothing
            Just (Present v) => Just v)
