import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as IV
import Data.Vector ( (!) )
import Control.Monad ( forM_ )

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
    t <- IV.freeze tree
    c <- IV.freeze costs
    return (t, c)

solve t c overall_sum = snd (snd (aux root 0))
    where
        root = head (t!0)
        aux n acc = (sum_curr, r)
            where
                new_acc = acc + c!n
                (sums, rs) = unzip (map (\x -> aux x new_acc) (t!n))
                
                sum_children = sum sums
                sum_curr = sum_children + c!n
                sum_above = overall_sum - sum_curr

                current_r = (maximum (sum_above:sums), n)
                r = minimum (current_r:rs)

main :: IO ()
main = do
    n_str <- C.getLine
    let n = maybe 0 fst (C.readInt n_str)
    (t, c) <- readInput n
    print (solve t c (sum c))
