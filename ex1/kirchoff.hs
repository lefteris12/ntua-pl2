import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as IV
import Data.Vector ( (!) )
import Control.Monad ( forM_ )
import Data.List ( foldl' )

listToTuple :: [C.ByteString] -> (Integer, Int)
listToTuple [a, b] = cost `seq` parent `seq` (cost, parent)
    where
        cost = maybe 0 fst (C.readInteger a)
        parent = maybe 0 fst (C.readInt b)
listToTuple _ = (0, 0)

readInput n = do
    tree <- MV.replicate (n + 1) ([] :: [Int])
    costs <- MV.new (n + 1)
    MV.write costs 0 0
    forM_ [1..n] (\i -> (do
        line <- C.getLine
        let ws = line `seq` C.words line
        let (c, p) = ws `seq` listToTuple ws
        c `seq` MV.write costs i c
        p `seq` MV.modify tree (i :) p
        ))
    t <- IV.unsafeFreeze tree
    c <- IV.unsafeFreeze costs
    return (t, c)

solve :: IV.Vector [Int] -> IV.Vector Integer -> Integer -> Int
solve t c overall_sum = overall_sum `seq` snd (snd (aux root))
    where
        root = head (t!0)
        aux n = sum_curr `seq` r `seq` (sum_curr, r)
            where
                (sum_children, max_sum, min_rs) = foldl' update (0, -1, (overall_sum, -1)) (t!n)
                update (pr_s, pr_m_s, pr_r) child = (pr_s + s, max pr_m_s s, min pr_r r) 
                    where (s, r) = aux child

                sum_curr = sum_children + c!n
                sum_above = overall_sum - sum_curr

                current_r = (max max_sum sum_above, n)
                r = current_r `seq` min min_rs current_r

main :: IO ()
main = do
    n_str <- C.getLine
    let n = maybe 0 fst (C.readInt n_str)
    (t, c) <- readInput n
    print (solve t c (sum c))
