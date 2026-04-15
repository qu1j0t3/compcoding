-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- References:
-- * Haddock markup https://haskell-haddock.readthedocs.io/latest/markup.html#
-- * Style https://wiki.haskell.org/Programming_guidelines#Good_Programming_Practice

-- Minimum coins, example used by "Tech with Nikola" video:
-- https://youtu.be/Hdr64lKQ3e4?t=795

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe

-- This is a successful dynamic programming implementation
-- that constructs all solutions from smallest subproblem towards target problem
-- and uses memoisation to reduce repeated work.

-- Notes:
-- * the map key is "target sum", and each sum is therefore a subproblem

-- Example: Find minimum coins from [1, 4, 5] needed to make a sum of 13
--          Some possible paths from 13 to our base case are
--            13->(5)->8->(5)->3->(1,1,1)->0         (5 coins)
--            13->(4)->9->(5)->4->(4)->0             (3 coins)
--            13->(1)->12->(5)->7->(5)->2->(1,1)->0  (5 coins)


-- |Naive recursive solver for minimum coints.
-- |This is excruciatingly slow for even small problems (e.g. minCoinsNaive [1,2,5,10] 30)
-- |because of the exponential cost of branching for every coin
-- |to evaluate every subproblem.
minCoinsNaive :: [Int] -> Int -> Maybe Int
minCoinsNaive _ 0 = Just 0
minCoinsNaive coins target =
  -- try each coin with solutions to subproblem that results after subtracting the coin
  let possibleCoins = filter (<= target) coins
  in case catMaybes (fmap (\ c -> minCoinsNaive coins (target-c)) possibleCoins) of
       [] -> Nothing
       cs -> Just $ foldl1 (min) (map (1+) cs)


-- |Find map of subproblems to coin counts; memoised version
minCoins :: [Int]           -- ^set of possible coins
            -> Int          -- ^target sum
            -> IntMap Int
minCoins coins target =
  foldl' (\ memo i ->         -- for each target sum
            foldl' (\ m c ->  -- for each coin that is not greater than the target sum
                      -- we look up for the current subproblem `i`
                      -- because it might have a solution that uses an earlier coin!
                      -- then we lookup an earlier solution after using THIS coin (subproblem = i-c)
                      -- take the minimum of each that exists and add it to the map at subproblem = i
                      case (catMaybes [IntMap.lookup i m, fmap (1+) (IntMap.lookup (i-c) m)]) of
                        []    -> m  -- nothing new to add to map
                        [a]   -> IntMap.insert i a m
                        [a,b] -> IntMap.insert i (min a b) m )
                   memo
                   (filter (<= i) coins) )
         (IntMap.singleton 0 0)
         [1..target]

-- Test using ghci:

-- % ghci mincoins
-- GHCi, version 9.14.1: https://www.haskell.org/ghc/  :? for help
-- [1 of 2] Compiling Main             ( mincoins.hs, interpreted )[main]
-- Ok, one module loaded.
-- ghci> minCoins [1,4,5] 13
-- fromList [(0,0),(1,1),(2,2),(3,3),(4,1),(5,1),(6,2),(7,3),(8,2),(9,2),(10,2),(11,3),(12,3),(13,3)]
