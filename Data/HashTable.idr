module Data.HashTable
import Data.IOArray
import Data.IORef
%default total

-------------
-- Summary --

export data HashTable: Type -> Type
export newHashTable: Eq v => (v -> Int) -> {default 32 size: Int} -> HasIO m => m (HashTable v)
export (.insert): HashTable v -> HasIO m => v -> m Bool
export (.member): HashTable v -> HasIO m => v -> m Bool
export (.delete): HashTable v -> HasIO m => v -> m Bool

------------
-- Detail --

data Probe v = Present v | Deleted

record HashTable (v: Type) where
   constructor HashArray
   {auto Eq: Eq v}
   hash: v -> Int
   load: IORef Int
   array: IORef (IOArray (Probe v))

Show v => Show (Probe v) where
   show (Present v) = "Present " ++ show v
   show Deleted = "Deleted"

export debug: Show v => HashTable v -> IO ()
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
   HashArray hash <$> newIORef 0 <*> newIORef array

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
            Just (Present e) => if (==) v e @{ht.Eq}
                                then pure False
                                else assert_total$ probe (probe_offset+1)
            Just  Deleted    =>      assert_total$ probe (probe_offset+1)
   probe 0

(.member) ht v = let
   hashed = ht.hash v
   probe: Int -> m Bool
   probe probe_offset = do
      array <- readIORef ht.array
      if probe_offset >= max array
         then pure False
         else do
            Just p <- readArray array ((hashed + probe_offset) `mod` max array)
               | Nothing => pure False
            case p of
               Present e => if (==) v e @{ht.Eq}
                            then pure True
                            else assert_total$ probe (probe_offset+1)
               Deleted   =>      assert_total$ probe (probe_offset+1)
   in probe 0

(.delete) ht v = let
   hashed = ht.hash v
   probe: Int -> m Bool
   probe probe_offset = do
      array <- readIORef ht.array
      let array_pos = (hashed + probe_offset) `mod` max array
      if probe_offset >= max array
         then pure False
         else do
            Just p <- readArray array array_pos
               | Nothing => pure False
            case p of
               Present e => if (==) v e @{ht.Eq}
                            then do
                              ignore$ writeArray array array_pos Deleted
                              modifyIORef ht.load (\l => l-1)
                              pure True
                            else assert_total$ probe (probe_offset+1)
               Deleted   =>      assert_total$ probe (probe_offset+1)
   in probe 0
