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
-- each move is (a,b) -> to either (a+1,b) or (a,b+1)
-- until reaching (m,n) (size of lattice)
-- subproblems are obviously indexed by position (a,b)

-- since all paths have the same start and end point
-- (there is no optimisation problem),
-- the task is just to count possible paths.

-- Naive recursive; exponential cost O(2^n) because evaluating
-- each problem requires the evaluation of two subproblems.
-- Sequence: https://oeis.org/A000984

moves :: Int -> Int -> Int
moves 0 0 = 1
moves a b = (if a > 0 then moves (a-1) b else 0) +
            (if b > 0 then moves a (b-1) else 0)

type SolutionMap = Map (Int,Int) Integer

-- |Memoising evaluator. If solution is in map, return it, and unchanged map.
-- |otherwise, call self for subproblems and return solution with updated map.
movesMStep :: SolutionMap -> Int -> Int -> (Integer,SolutionMap)
movesMStep m a b =
  case Map.lookup (a,b) m of
    Just solution -> (solution,m)
    Nothing -> let (s,m')   = if a > 0 then movesMStep m (a-1) b else (0,m)
                   (s',m'') = if b > 0 then movesMStep m' a (b-1) else (0,m')
                   sol = s + s'
               in (sol, Map.insert (a,b) sol m'')

movesM :: Int -> Int -> Integer
movesM a b = fst $ movesMStep (Map.singleton (0,0) 1) a b


-- Variation: related to "counting Dyck words", per Theo Honahan:
-- https://bsky.app/profile/theohonohan.bsky.social/post/3lrgkx6bgd22o
-- where a must be < b
-- Sequence: the Catalan numbers https://oeis.org/A000108

movesHalf :: Int -> Int -> Int
movesHalf 0 0 = 1
movesHalf a b = (if a > 0 then movesHalf (a-1) b else 0) +
                (if b > 0 && b > a then movesHalf a (b-1) else 0)



main :: IO ()
main = getLine >>= (print . (\ [a,b] -> movesM a b) . (map read) . words)
