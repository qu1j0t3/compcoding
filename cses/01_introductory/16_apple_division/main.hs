-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Apple Division
-- https://cses.fi/problemset/task/1623

-- n apples with known weights. Your task is to divide the apples into two groups
-- so that the difference between the weights of the groups is minimal.
-- Input
-- The first input line has an integer n: the number of apples.
-- The next line has n integers p_1,p_2,\dots,p_n: the weight of each apple.
-- Output
-- Print one integer: the minimum difference between the weights of the groups.
-- Constraints

-- 1 \le n \le 20
-- 1 \le p_i \le 10^9

-- Example
-- Input:
-- 5
-- 3 2 7 4 1

-- Output:
-- 1

-- Explanation: Group 1 has weights 2, 3 and 4 (total weight 9), and group 2 has weights 1 and 7 (total weight 8).


-- distribute apples into the two boxes

-- "Is there a way to make the difference smaller"
-- given heavier box is L and lighter box is S
-- difference d = L-S
-- It's impossible to reduce the difference by moving an apple from smaller to larger
-- but if there is an apple a in L that is < d
-- can move the largest such from L to S

import Data.List


solve :: [Int] -> [Int] -> Int
solve [] [a] = a  -- degenerate case of one apple
solve a b = let aw = sum a
                bw = sum b
            in if aw < bw then solve' a b aw bw
               else if aw > bw then solve' b a bw aw
               else 0

solve' :: [Int] -> [Int] -> Int -> Int -> Int
solve' s l sw lw =
  case filter (< (lw - sw)) l of
    [] -> lw - sw
    as -> let a = maximum as
          in solve (a:s) (delete a l)

main :: IO ()
main = getLine >> getLine >>= (print . (solve []) . (map read) . words)
