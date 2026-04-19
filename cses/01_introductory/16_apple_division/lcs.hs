-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- References:
-- * Haddock markup https://haskell-haddock.readthedocs.io/latest/markup.html#
-- * Style https://wiki.haskell.org/Programming_guidelines#Good_Programming_Practice
-- * Rules of thumb for folds https://wiki.haskell.org/Foldr_Foldl_Foldl%27
-- * Jelvis' https://jelv.is/blog/Lazy-Dynamic-Programming/

-- Longest Common Subsequence example from
-- https://web.cs.dal.ca/~nzeh/Teaching/3137/haskell/standard_containers/arrays/lcs/

-- To test,
-- % ghci lcs
-- ghci> lcsM [1,1,2,5,7,5,4,2,6] [2,7,5,6,2,2,5]
-- [2,7,5,6]

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- Naive recursive implementation - exponential behaviour
lcs :: Eq a => [a] -> [a] -> [a]

lcs [] _ = []
lcs _ [] = []
lcs (a:as) (b:bs) | a == b    = a:(lcs as bs)
                  | otherwise = let lcs1 = lcs (a:as) bs
                                    lcs2 = lcs as (b:bs)
                                in if length lcs1 > length lcs2 then lcs1 else lcs2


-- |Memoising evaluator. If solution is in map, return it, and unchanged map.
-- |otherwise, call self for subproblems and return solution with updated map.

-- We need to find a way to index unique subproblems
-- probably should be "elements dropped from start"?
-- this initially starts at (0,0) (two input strings are complete)
-- and counts up to (a,_) or (_,b) which are all empty list if a and b are the respective input lengths

lcsM :: Eq a => [a] -> [a] -> [a]

lcsM input1 input2 = fst $ lcsMStep Map.empty input1 input2 0 0
  where lcsMStep :: Eq a => Map (Int,Int) [a] -> [a] -> [a] -> Int -> Int -> ([a], Map (Int,Int) [a])
        lcsMStep m [] _ _ _ = ([],m)   -- the base cases: lcs when either list is empty, is empty list
        lcsMStep m _ [] _ _ = ([],m)
        lcsMStep m (a:as) (b:bs) i j = -- (i,j) = index to subproblem: number of elements dropped before each list
          case Map.lookup (i,j) m of   -- if solution is in the map,
            Just soln -> (soln,m)      -- return it
            -- otherwise, solve subproblem:
            -- if heads are equal, accumulate head into sequence and continue on both lists without their heads
            Nothing -> let (soln,updatedMap) =
                             if a == b then let (s,m')   = lcsMStep m as bs (i+1) (j+1) in (a:s,m')
                -- if heads are unequal, try one step deeper on both lists (branching) and take longest returned sequence
                                       else let (s1,m')  = lcsMStep m (a:as) bs i (j+1)
                                                (s2,m'') = lcsMStep m' as (b:bs) (i+1) j
                                            in if length s1 > length s2 then (s1,m'') else (s2,m'')
                       in (soln, Map.insert (i,j) soln updatedMap) -- add solved subproblem to the map


-- Read two lines as strings then print longest common subsequence of the strings
main :: IO ()

main = getLine >>= (\ a ->
       getLine >>= (\ b ->
         (putStrLn $ "1:   " ++ a) >>
         (putStrLn $ "2:   " ++ b) >>
         (putStrLn $ "LCS: " ++ (lcsM a b))))



