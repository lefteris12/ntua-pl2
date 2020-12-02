import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)

-- Add your solutions here!

t = Node 'a' [ Node 'b' [ Node 'd' [Node 'i' []]
                        , Node 'e' [Node 'j' [], Node 'k' []]
                        ]
             , Node 'c' [ Node 'f' [Node 'l' [], Node 'm' []]
                        , Node 'g' []
                        , Node 'h' [Node 'n' []]
                        ]
             ]

tm = Node 'a' [ Node 'c' [ Node 'h' [ Node 'n' []]
                         , Node 'g' []
                         , Node 'f' [Node 'm' [], Node 'l' []]
                         ]
              , Node 'b' [ Node 'e' [Node 'k' [], Node 'j' []]
                         , Node 'd' [Node 'i' []]
                         ]
              ]

test_correctness msg testcases = do
  putStr $ msg ++ ": " ++ (if and testcases then "OK" else "FAIL!!!") ++ "\n"

test_complexity msg range f = forM_ range $ \n -> do
  putStr $ msg ++ " with size " ++ show n ++ ", "
  timeIt $ evaluate $ f n

main = do
  test_correctness "mirror correctness" $
    [mirror t == tm] ++
    [mirror (mirror t) == t | n <- [0..100], let t = leftish n] ++
    [mirror (mirror t) == t | n <- [0..15], let t = fibtree n]
  test_correctness "fringe_naive correctness" $
    [fringe_naive t == "ijklmgn"] ++
    [fringe_naive (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe_naive (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe_naive leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe_naive . leftish
  test_correctness "fringe correctness" $
    [fringe t == "ijklmgn"] ++
    [fringe (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe . leftish
  test_correctness "same_fringe correctness" $
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]] ++
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]]
  test_complexity "mirror fibtree_naive" [20, 25, 30, 32] $ \n ->
    let t = fibtree_naive n in mirror (mirror t) == t
  test_complexity "mirror fibtree" [20, 25, 30, 32] $ \n ->
    let t = fibtree n in mirror (mirror t) == t
  test_complexity "same_fringe fibtree_naive" [20, 25, 30, 32] $ \n ->
    same_fringe (fibtree_naive n) (fibtree_naive (n+1))
  test_complexity "same_fringe fibtree" [20, 25, 30, 32, 34, 36] $ \n ->
    same_fringe (fibtree n) (fibtree (n+1))

-- (a)
leftish :: Int -> Tree Int
leftish 0 = Node 0 []
leftish n = Node n [leftish (n-1), Node n []]

-- (b)
mirror :: Tree a -> Tree a
mirror (Node n ts) = Node n (reverse ts')
  where ts' = map mirror ts

-- (c)
-- Η πολυπλοκότητα της fringe_naive είναι O(n^2) όπου n το πλήθος των φύλλων του δέντρου
-- Αυτό συμβαίνει γιατί στην αναδρομή κάνουμε append σε λίστες και το append έχει γραμμική πολυπλοκότητα
fringe_naive :: Tree a -> [a]
fringe_naive (Node n []) = [n] 
fringe_naive (Node n (t:ts)) = fringe_naive t ++ concatMap fringe_naive ts

-- (d)
-- Η πολυπλοκότητα της fringe είναι O(n) όπου n το πλήθος των φύλλων του δέντρου
-- επειδή τώρα χρησιμοποιούμε accumulator και τον τελεστή cons που έχει σταθερή πολυπλοκότητα 
fringe :: Tree a -> [a]
fringe t = reverse $ aux t [] []
  where aux (Node _ (t:ts)) nextTrees acc = aux t (ts ++ nextTrees) acc
        aux (Node n []) (nt:nts) acc = aux nt nts (n:acc)
        aux (Node n []) [] acc = n:acc

-- (e)
-- Η πολυπλοκότητα της same_fringe είναι O(min(n1, n2)) όπου n1, n2 τα πλήθη των φύλλων του δέντρου.
-- Στην περίπτωση που τα δύο περιγράμματα διαφέρουν, μόνο τα στοιχεία τους μέχρι το πρώτο διαφορετικό υπολογίζονται.
-- Αυτό συμβαίνει επειδή η Haskell είναι lazy, κάνει δηλαδή την αποτίμηση μόνο όταν χρειαστεί.
-- Άρα οι δύο λίστες δεν είναι απαραίτητο ότι θα αποτιμηθούν ολόκληρες.
same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe t1 t2 = fringe t1 == fringe t2

-- (f)
-- Σε μια eager γλώσσα δε θα ήταν τόσο εύκολο να πετύχουμε μια εξίσου αποδοτική same_fringe.
-- Σε μια τέτοια γλώσσα τα περιγράμματα θα έπρεπε να αποτιμηθούν ολόκληρα ακόμα και αν διαφέρουν.
-- Σε αυτή την περίπτωση θα έπρεπε να γραφεί μια same_fringe που εκτελεί τον αλγόριθμο της fringe παράλληλα και για τα δύο δέντρα
-- και αν σε κάποιο σημείο βρει διαφορετικά στοιχεία στα περιγράμματα σταματάει.

-- (g)
-- Το Tn που κατασκευάζει η fibtree_naive καταλαμβάνει μνήμη O(Fn) όπου Fn ο n-οστός αριθμός Fibonacci
fibtree_naive :: Int -> Tree Int
fibtree_naive n = Node (fib n) ts
  where
    ts = if n >= 2 then [fibtree_naive (n-1), fibtree_naive (n-2)]
         else []
    fib n
      | n >= 2 = fib (n-1) + fib(n-2)
      | otherwise = n

-- (h)
fibtree :: Int -> Tree Int
fibtree 0 = Node 0 []
fibtree 1 = Node 1 []
fibtree n = aux n_fibs (fibtree 0) (fibtree 1)
  where
    aux [] tn_2 tn_1 = tn_1
    aux (f:fs) tn_2 tn_1 = aux fs tn_1 tn
      where
        tn = (Node f [tn_1, tn_2])
    n_fibs = take (n-1) $ drop 2 fibs
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- (i)
-- Η mirror, επειδή είναι αναδρομική, επισκέπτεται όλους τους κόμβους του αρχικού δέντρου και τους αντιγράφει (αφού φροντίσει να αντιστρέψει τη σειρά εμφάνισης των παιδιών).
-- Επομένως ακόμα και αν το δέντρο που πάρει ως όρισμα προκύπτει από την fibtree και καταλαμβάνει μνήμη O(n), το αποτέλεσμα θα καταλαμβάνει μνήμη O(Fn). 
