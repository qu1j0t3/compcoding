-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Increasing Array
-- https://cses.fi/problemset/task/1094

-- import Data.List


-- Find all values that are less than their predecessor.
-- The least change is to make them equal to their predecessor,
-- so the minimum change steps to achieve a nondecreasing sequence
-- is the positive sum of all those differences.

-- We can slide a 2-value window along the list.
-- This can be done by zip'ing the input
-- with itself, less its head value.


sumGaps :: [Int] -> Int

sumGaps series = sum [ a-b | (a,b) <- zip series (drop 1 series), b < a ]


increasing :: Int -> IO Int

increasing count = fmap (sumGaps . (map read) . (take count) . words) getLine


main :: IO ()

main = getLine >>= (increasing . read) >>= print

