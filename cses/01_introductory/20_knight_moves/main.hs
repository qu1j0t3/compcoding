-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment
-- import System.Random

-- import System.Exit
-- import System.IO

-- import Data.Foldable
-- import Data.List
-- import Data.Bits

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe ( catMaybes )

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


-- | Naive recursive solution
-- Compiled with ghc, this is excessively slow when n=6
moves :: Int -> Int -> Int -> Set (Int,Int) -> Maybe Int
moves _ 1 1 _   = Just 0
moves n i j acc =
  case catMaybes [ moves n (i+a) (j+b) (Set.insert (i+a,j+b) acc)
                   | a <- [-2..2], b <- [-2..2], abs a + abs b == 3,
                     i+a >= 1, i+a <= n, j+b >= 1, j+b <= n, Set.notMember (i+a,j+b) acc ] of
    [] -> Nothing
    xs -> Just $ 1 + minimum xs


solve :: Int -> [[Maybe Int]]
solve n = [[moves n i j (Set.singleton (i,j)) | j <- [1..n]] | i <- [1..n]]



-- N.B. THIS IS WRONG BECAUSE IT'S DEPTH FIRST.
--      It follows one single path until the board is full,
--      with move counts up to 55
movesMdfs m n i j moveCount | i<1 || i>n || j<1 || j>n || Map.member (i,j) m = m
                            | otherwise = foldr (\ (a,b) m' -> movesMdfs m' n (i+a) (j+b) (moveCount+1))
                                                (Map.insert (i,j) moveCount m)
                                                [(a,b) | a <- [-2..2], b <- [-2..2], abs a + abs b == 3]


-- This gets uncomfortably slow by low 20's:

type SolutionMap = Map (Int,Int) Int

-- | Starting from origin 1,1, populate map with move counts, spreading outwards
--   from the current square by using every knight move to an unvisited square.
movesM :: SolutionMap  -- ^ solutions computed so far, as a map from position to move count
       -> Int          -- ^ rows in board
       -> Int          -- ^ columns in board
       -> [(Int,Int)]  -- ^ next squares to explore
       -> Int          -- ^ move count (recursion depth)
       -> SolutionMap  -- ^ return updated map

movesM m _ _ [] _ = m
movesM m n n' squares moveCount =
  let validMoves = [ (i',j') | (i,j) <- squares,
                               a <- [-2..2], b <- [-2..2], abs a + abs b == 3,
                               let i'=i+a, let j'=j+b,
                               i' >= 1, i' <= n, j' >= 1, j' <= n',
                               Map.notMember (i',j') m ]
      -- update map with counts for this depth
      newMap = foldr (`Map.insert` moveCount) m validMoves
  in movesM newMap n n' validMoves (moveCount+1)


-- | Produce 2D solution for board of size n x n'
solveM2 :: Int -> Int -> [[Maybe Int]]
solveM2 n n' = [[Map.lookup (i,j) m | j <- [1..n']] | i <- [1..n]]
              where m = movesM (Map.singleton (1,1) 0) n n' [(1,1)] 1

-- | Solve square board of size n
solveM n = solveM2 n n


-- | Produce string for board square including ANSI colour code based on
-- | count in the square. Also pad short numbers so columns align.
prettyWidth = 2
prettyPrint (Just c) = replicate (prettyWidth - length (show c)) ' '
                       ++ "\27[3" ++ show (1 + c `mod` 8) ++ "m"
                       ++ show c
                       ++ "\27[0m"
prettyPrint Nothing  = replicate (prettyWidth - 1) ' ' ++ "-"


plainPrint (Just c) = show c  -- even though the problem doesn't call for it,
plainPrint Nothing  = "-"     -- this allows us to correctly print boards smaller than 4x4


-- | Running with args n n' produces a colour coded board of size n x n'
-- | Without args, expects a line on standard input with a square board size (CSES judge format)
main :: IO ()
main = getArgs >>= (
  \case []     -> getLine >>= (\ input -> mapM_ (putStrLn . unwords . map plainPrint) (solveM $ read input))
        [n,n'] -> mapM_ (putStrLn . unwords . map prettyPrint) (solveM2 (read n) (read n')))