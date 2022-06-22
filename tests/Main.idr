import Data.HashTable

assert: Applicative m => Bool -> String -> m ()
assert test message = if (not test)
   then assert_total$ idris_crash$ "assertion failed: \{message}"
   else pure ()

main: IO ()
main = do
   ht <- newHashTable id {v = Int, size = 4}
   ignore$ ht.insert 1
   ignore$ ht.insert 5
   ignore$ ht.insert 9
   ignore$ ht.insert 13
   assert !(ht.member  1) "member 1"
   assert !(ht.member  5) "member 5"
   assert !(ht.member 13) "member 13"
   ignore$ ht.delete 5
   debug ht
   assert (not !(ht.member 5)) "no member 5 after delete"
