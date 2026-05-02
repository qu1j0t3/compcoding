-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.Exit
import System.IO

import Data.Foldable
import Data.List
-- import Data.Maybe
-- import Data.IntSet (IntSet, fromList)
-- import qualified Data.IntSet as IntSet


-- Solution to CSES Introductory Problems Mex Grid Construction
-- https://cses.fi/problemset/task/3419


-- Your task is to construct an n \times n grid where each square has the smallest nonnegative integer that does not appear to the left on the same row or above on the same column.
-- Input
-- The only line has an integer n.
-- Output
-- Print the grid according to the example.
-- Constraints

-- 1 \le n \le 100

-- Example
-- Input:
-- 5

-- Output:
-- 0 1 2 3 4
-- 1 0 3 2 5
-- 2 3 0 1 6
-- 3 2 1 0 7
-- 4 5 6 7 0


square :: Int -> Int -> Int
square 0 j = j
square i 0 = i
square i j | i == j = 0
           | i > j = square j i -- solution is symmetric across diagonal
           | otherwise =  -- i < j
              let filled = [square i' j | i' <- [0..i-1]] ++ [square i j' | j' <- [0..j-1]]
                  v = head (filter (\ e -> notElem e filled) [1..])
              in v


solve :: Int -> [[Int]]
solve n = [[square i j | j <- [0..n-1]] | i <- [0..n-1]]


main :: IO ()
main = getLine >>= (\ input -> sequence_ $ fmap putStrLn (map (\r -> unwords $ map show r) (solve $ read input)))
