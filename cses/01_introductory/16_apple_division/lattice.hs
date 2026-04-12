-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- References:
-- * Haddock markup https://haskell-haddock.readthedocs.io/latest/markup.html#
-- * Style https://wiki.haskell.org/Programming_guidelines#Good_Programming_Practice
-- * Rules of thumb for folds https://wiki.haskell.org/Foldr_Foldl_Foldl%27

-- "Maze", example used by "Tech with Nikola" video:
-- https://youtu.be/Hdr64lKQ3e4?t=795


import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

-- base case (0,0) (corner of lattice)
-- paths start at (m,n) - opposite corner of lattice
-- each move is (a,b) -> to either (a-1,b) or (a,b-1)
-- until reaching (0,0).
-- subproblems are obviously indexed by position (a,b)

-- since all paths have the same start and end point
-- (there is no optimisation problem),
-- the task is just to count possible paths.

-- Naive recursive; exponential cost O(2^n) because evaluating
-- each problem requires the evaluation of two subproblems.
-- Sequence: https://oeis.org/A000984

paths :: Int -> Int -> Int
paths 0 0 = 1
paths a b = (if a > 0 then paths (a-1) b else 0) +
            (if b > 0 then paths a (b-1) else 0)

type SolutionMap = Map (Int,Int) Integer

-- |Memoising evaluator. If solution is in map, return it, and unchanged map.
-- |otherwise, call self for subproblems and return solution with updated map.
pathsMStep :: SolutionMap -> Int -> Int -> (Integer,SolutionMap)
pathsMStep m a b =
  case Map.lookup (a,b) m of
    Just solution -> (solution,m)
    Nothing -> let (s,m')   = if a > 0 then pathsMStep m (a-1) b else (0,m)
                   (s',m'') = if b > 0 then pathsMStep m' a (b-1) else (0,m')
                   sol = s + s'
               in (sol, Map.insert (a,b) sol m'')

pathsM :: Int -> Int -> Integer
pathsM a b = fst $ pathsMStep (Map.singleton (0,0) 1) a b


-- Variation: related to "counting Dyck words", per Theo Honahan:
-- https://bsky.app/profile/theohonohan.bsky.social/post/3lrgkx6bgd22o
-- where a must be < b
-- Sequence: the Catalan numbers https://oeis.org/A000108

pathsHalf :: Int -> Int -> Int
pathsHalf 0 0 = 1
pathsHalf a b = (if a > 0 then pathsHalf (a-1) b else 0) +
                (if b > 0 && b > a then pathsHalf a (b-1) else 0)



main :: IO ()
main = getLine >>= (print . (\ [a,b] -> pathsM a b) . (map read) . words)
