import qualified Data.ByteString.Char8 as C

-- import Data.Map (Map)
import Data.Map.Strict (Map)
-- import qualified Data.Map as Map
import qualified Data.Map.Strict as Map

import Data.List (foldl')

-- Every node contains number, cost and list of subtrees
data Tree = Node Int Integer [Tree]
    deriving (Eq, Show, Read)

getTree :: [(Integer, Int)] -> Tree
getTree lst = getSubTree (head (Map.findWithDefault [] 0 childmp))
    where
        childmp = getChildrenMap lst
        costmp = getCostMap lst

        getSubTree :: Int -> Tree
        getSubTree n = Node n (Map.findWithDefault 0 n costmp) (getSubTrees n)
        getSubTrees :: Int -> [Tree]
        getSubTrees n = map getSubTree (Map.findWithDefault [] n childmp)

getCostMap :: [(Integer, Int)] -> Map Int Integer
getCostMap lst = Map.fromList (zip [1..length lst] (map fst lst)) 

getChildrenMap :: [(Integer, Int)] -> Map Int [Int]
getChildrenMap lst = aux lst Map.empty 1
    where
        aux [] m _ = m
        aux ((_c, parent):xs) m i = aux xs m' (i+1)
            where m' = Map.insertWith (\new old -> head new : old) parent [i] m


-- TODO very slow
getSum :: Tree -> Map Int Integer -> (Integer, Map Int Integer)
getSum (Node n c ts) mp = 
    if Map.member n mp then (Map.findWithDefault 0 n mp, mp)
    else (c + s, Map.insert n (c + s) mp')
        where
            (s, mp') = foldl foldFunc (0, mp) ts
            foldFunc (s', mp'') t = (s' + s'', mp''')
                where
                    (s'', mp''') = getSum t mp''


-- LAZY?
-- getSum :: Tree -> Integer
-- getSum (Node _ c []) = c
-- getSum (Node _ c ts) = c + sum (map getSum ts)


getNeighborsMax :: Tree -> Integer -> Map Int Integer -> Integer
getNeighborsMax tree@(Node _ _ ts) summ mem = maximum (pSum:cSums)
    where
        pSum = summ - fst (getSum tree mem)
        cSums = map (\t -> fst (getSum t mem)) ts

solve :: Tree -> Int
solve t = snd (aux t (summ, 0))
    where
        (summ, mem) = getSum t Map.empty
        cost t = getNeighborsMax t summ mem
        aux (Node _ _ []) sofar = sofar 
        aux t@(Node n _ ts) sofar = auxs ts sofar' 
            where
                sofar' = min sofar (cost t, n)
        auxs [] sofar = sofar
        auxs (t:ts) sofar = auxs ts (aux t sofar)

main :: IO ()
main = do
    _ <- C.getLine
    rest <- C.getContents
    let listToTuple [a,b] = (maybe 0 fst (C.readInteger a), maybe 0 fst (C.readInt b))
        lists = [listToTuple l | l <- map C.words (C.lines rest), length l == 2] 
    print $ solve $ getTree lists

--delete
-- t = getTree [(10,3),(10,0),(10,2),(20,3),(20,4)]