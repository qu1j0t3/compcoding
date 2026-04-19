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

let t = total divide by 2
look at possible sums
get the two closest to t

  test data: [10,16,2,19,6,13,7,13,17,1]
  [10,2,1,2,3,1,1,5,4]

-}

solve :: [Int] -> Int
solve xs = let halfDown = (sum xs) `div` 2
               (a1,a2) = lsum xs halfDown
           in abs (a1-a2)

-- |Compute the largest subset sum from list that's <= mx
-- |Return tuple of the subset sum and the sum of complemented subset
lsum :: [Int] -> Int -> (Int,Int)
lsum input mx =
  lstep input 0 0
  where lstep [] acc acc2     = (acc,acc2) -- base case, input used up
        lstep (x:xs) acc acc2 =
          let (a,a2)   = lstep xs acc (x+acc2) -- take element in acc2 and continue
              (a',a2') = lstep xs (x+acc) acc2 -- take in acc, continue
          in if acc+x > mx || a > a' then (a,a2) else (a',a2')


main :: IO ()
main = getLine >> getLine >>= (print . solve . (map read) . words)
