import Data.HashMap
import Data.HashTable

assert: Applicative m => Bool -> String -> m ()
assert test message = if (not test)
   then assert_total$ idris_crash$ "assertion failed: \{message}"
   else pure ()

main: IO ()
main = do

   ht <- newHashTable id {v = Int, size = 4}
   assert !(ht.insert  1) "insert 1"
   assert !(ht.insert  5) "insert 5"
   assert !(ht.insert  9) "insert 9"
   assert !(ht.insert 13) "insert 13"
   assert !(ht.member  1) "member 1"
   assert !(ht.member  5) "member 5"
   assert !(ht.member 13) "member 13"
   assert !(ht.delete  5) "delete 5"
   assert (not !(ht.member 5)) "no member 5 after delete"
   debug ht

   hm <- newHashMap id {k = Int, v = String}
   assert !(hm.insert 1 "Hello")  "insert 1"
   assert !(hm.insert 2 ", ")     "insert 2"
   assert !(hm.insert 3 "World!") "insert 3"
   assert ( !(hm.lookup 3) == Just "World!" ) "lookup 3"
   assert ( !(hm.lookup 33) == Nothing ) "absent 33"
