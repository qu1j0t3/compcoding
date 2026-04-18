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

-- how many ways to distribute? it's a binary vector
-- 000...000   all apples in box 0
-- 111...111   all in box 1
-- (every other bit string)  some distribution between non empty boxes
-- (if there is > 1 apple then neither box will be empty)
-- i.e. 2^n ways - complexity is exponential in the number of apples
-- if we try to work out the answer by brute force.

-- We need to be efficient for problems up to 20 apples (2^20 arrangements)


-- there is a clever way of getting 'sum of subset'
-- See Knapsack Problems, page 83, Competitive Programmer’s Handbook, Laaksonen

{-
How about this plan:
Distribute into 2 boxes
Let d be the difference
using a sub-exponential 'sum of subsets' method,
look for a subset in the larger group that is somewhere between d/2 and d-1
if there is one, move the subset to the smaller group

  test data: [10,16,2,19,6,13,7,13,17,1]
-}


import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


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

-- |Powerset of a list (all subsets, but allowing duplicate elements)
-- (This is actually in Data.List as `subsequences`)
ps :: [a] -> [[a]]
ps [] = [[]]
ps (x:xs) = (ps xs) ++ map (x:) (ps xs)

-- |Get all unique sums of a list of weights
uniqueSums :: [Int] -> IntSet
uniqueSums xs = IntSet.fromList $ map sum $ ps xs

-- |Get unique differences between each partitioning into two boxes
uniqueDiffs :: [Int] -> IntSet
uniqueDiffs xs =
  let sums = map sum $ ps xs
  in IntSet.fromList $ map (\ (a,b) -> abs (a-b)) $ zip sums (reverse sums)

main :: IO ()
main = getLine >> getLine >>= (print . (solve []) . (map read) . words)
