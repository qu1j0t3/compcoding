-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

{-# LANGUAGE LambdaCase #-}

-- import System.Environment
-- import System.Exit
-- import System.IO

-- import Data.Foldable
-- import Data.List
-- import Data.Bits

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

-- import Data.IntSet (IntSet, fromList)
-- import qualified Data.IntSet as IntSet


-- Solution to CSES Introductory Problems Knight Moves Grid
-- https://cses.fi/problemset/task/3217

-- There is a knight on an n \times n chessboard. For each square, print the minimum number of moves the knight needs to do to reach the top-left corner.
-- Input
-- The only line has an integer n.
-- Output
-- Print the number of moves for each square.
-- Constraints

-- 4 \le n \le 1000

-- Example
-- Input:
-- 8

-- Output:
-- 0 3 2 3 2 3 4 5
-- 3 4 1 2 3 4 3 4
-- 2 1 4 3 2 3 4 5
-- 3 2 3 2 3 4 3 4
-- 2 3 2 3 4 3 4 5
-- 3 4 3 4 3 4 5 4
-- 4 3 4 3 4 5 4 5
-- 5 4 5 4 5 4 5 6

-- Haha https://oeis.org/A018837
-- & https://oeis.org/A183043


-- Naive recursive solution
-- Compiled with ghc, this is excessively slow when n=6
moves :: Int -> Int -> Int -> Set (Int,Int) -> Maybe Int
moves _ 1 1 _   = Just 0
moves n i j acc =
  case catMaybes [ fmap (1+) $ moves n (i+a) (j+b) (Set.insert (i+a,j+b) acc)
                   | a <- [-2..2], b <- [-2..2], abs a + abs b == 3,
                     i+a >= 1, i+a <= n, j+b >= 1, j+b <= n, Set.notMember (i+a,j+b) acc
                 ] of
    [] -> Nothing
    xs -> Just $ minimum xs


solve :: Int -> [[Maybe Int]]
solve n = [[moves n i j (Set.singleton (i,j)) | j <- [1..n]] | i <- [1..n]]


-- memoised version

type SolutionMap = Map (Int,Int) Int

movesM :: SolutionMap -> Int -> Int -> Int -> Set (Int,Int) -> (Maybe Int,SolutionMap)
movesM m _ 1 1 _       = (Just 0,m)
movesM m n i j visited | i<1 || j<1 || i>n || j>n || Set.member (i,j) visited = (Nothing,m)
                       | otherwise =
  case Map.lookup (i,j) m of
    Nothing ->
      let knightMoves = [ (i+a,j+b) | a <- [-2..2], b <- [-2..2], abs a + abs b == 3 ]
          visited' = Set.insert (i,j) visited
          (counts,m') = foldr (\ (i',j') (cs,mm) ->
                                case movesM mm n i' j' visited' of
                                  (Just c,newMap) -> (c+1:cs,newMap)
                                  (Nothing,newMap) -> (cs,newMap))
                              ([],m)
                              knightMoves
      in case counts of
        [] -> (Nothing,m')
        solutions -> let s = minimum solutions in (Just s, Map.insert (i,j) s m')
    sol -> (sol,m)


solveM :: Int -> [[Maybe Int]]
solveM n = [[fst $ movesM Map.empty n i j Set.empty | j <- [1..n]] | i <- [1..n]]


main :: IO ()
main = getLine >>= (\ input -> sequence_ $ fmap putStrLn (map (\r -> unwords $ map printSquare r) (solveM $ read input)))
  where printSquare (Just c) = show c  -- even though the problem doesn't call for it,
        printSquare Nothing  = "-"     -- this allows us to correctly print boards smaller than 4x4
