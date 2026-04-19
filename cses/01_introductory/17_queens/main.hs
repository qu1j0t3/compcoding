-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Chessboard and Queens
-- https://cses.fi/problemset/task/1624

-- Your task is to place eight queens on a chessboard so that no two queens are attacking each other. As an additional challenge, each square is either free or reserved, and you can only place queens on the free squares. However, the reserved squares do not prevent queens from attacking each other.

-- How many possible ways are there to place the queens?

-- Input
-- The input has eight lines, and each of them has eight characters. Each square is either free (.) or reserved (*).

-- Output
-- Print one integer: the number of ways you can place the queens.

-- Example
-- Input:

-- ........
-- ........
-- ..*.....
-- ........
-- ........
-- .....**.
-- ...*....
-- ........
-- Output:

-- 65


import Data.Array

solve :: [((Int,Int),Bool)] -> Int
solve input =
  let reserved = array ((1,1),(8,8)) input
  in sum [ if reserved!(i,j) then 1 else 0 | i <- [1..8], j <- [1..8] ]

readInput :: IO [((Int,Int),Bool)]
readInput = fmap mconcat -- flatten the 2-D input into a list of index/value tuples
                 (traverse (\ r -> -- traverse over 8 rows
                            fmap (\ line -> map (\ (c,b) -> ((r,c),b)) -- pair (row,col) tuples with reserved flags
                                                (zip [1..] (map (== '*') line))) -- add column index and test for '*'
                                 getLine) -- read each row as a line (string)
                           [1..8])

main :: IO ()
main = readInput >>= (print . solve)

