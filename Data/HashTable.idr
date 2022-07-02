module Data.HashTable
import Data.List {- catMaybes -}
import Data.Maybe {- isJust -}
import Data.IOArray {- newArray, readArray, writeArray -}
import Data.IORef {- newIORef, modifyIORef, readIORef, writeIORef -}
%default total

-------------
-- Summary --

export data HashTable: Type -> Type
export newHashTable: Eq v => (v -> Int) -> {default 32 size: Int} -> HasIO m => m (HashTable v)
export (.insert): HashTable v -> HasIO m => v -> m Bool
export (.member):   HashTable v ->  v          -> HasIO m => m Bool
export (.delete):   HashTable v ->  v          -> HasIO m => m Bool
export (.lookup):   HashTable v ->  v          -> HasIO m => m (Maybe v)

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

newHashTable hash = do
   array <- newArray size
   HashArray hash (==) <$> newIORef 0 <*> newIORef array

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

(.insert) ht v = do
   array <- readIORef ht.array
   currentLoad <- readIORef ht.load
   let loadFactor = cast (currentLoad+1) / cast (max array)
   let hashed = ht.hash v
   when (loadFactor > loadThreshold) (grow ht)
   array <- readIORef ht.array
   let probe: Int -> m Bool
       probe probe_offset = do
         let array_pos = (hashed + probe_offset) `mod` max array
         case !(readArray array array_pos) of
            Nothing => do
               ignore$ writeArray array array_pos (Present v)
               modifyIORef ht.load (+1)
               pure True
            Just (Present e) => if ht.equal v e
                                then pure False
                                else assert_total$ probe (probe_offset+1)
            Just  Deleted    =>      assert_total$ probe (probe_offset+1)
   probe 0

(.lookup) ht x = let
   hashed = ht.hash x
   probe: Int -> m (Maybe v)
   probe probe_offset = do
      array <- readIORef ht.array
      let array_pos = (hashed + probe_offset) `mod` max array
      if probe_offset >= max array
         then pure Nothing
         else case !(readArray array array_pos) of
                 Just (Present e) => if ht.equal x e
                                     then pure (Just e)
                                     else assert_total$ probe (probe_offset+1)
                 Just  Deleted   =>       assert_total$ probe (probe_offset+1)
                 Nothing         => pure Nothing
   in probe 0

(.member) ht v = isJust <$> ht.lookup v

(.delete) ht v = let
   hashed = ht.hash v
   probe: Int -> m Bool
   probe probe_offset = do
      array <- readIORef ht.array
      let array_pos = (hashed + probe_offset) `mod` max array
      if probe_offset >= max array
         then pure False
         else case !(readArray array array_pos) of
                 Just (Present e) => if ht.equal v e
                                     then do
                                       ignore$ writeArray array array_pos Deleted
                                       modifyIORef ht.load (\l => l-1)
                                       pure True
                                     else assert_total$ probe (probe_offset+1)
                 Just  Deleted    =>      assert_total$ probe (probe_offset+1)
                 Nothing          =>   pure False
   in probe 0

toList ht = do
   array <- readIORef ht.array {io=m}
   for [0 .. max array - 1] (readArray array)
      <&> mapMaybe (\case
            Nothing          => Nothing
            Just  Deleted    => Nothing
            Just (Present v) => Just v)
