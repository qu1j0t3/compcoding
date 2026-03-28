-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Coin Piles
-- https://cses.fi/problemset/task/1754

-- You have two coin piles containing a and b coins.
-- On each move, you can either remove one coin from the left pile
-- and two coins from the right pile, or two coins from the left pile and one coin from the right pile.
-- Your task is to efficiently find out if you can empty both the piles.
-- Input
-- The first input line has an integer t: the number of tests.
-- After this, there are t lines, each of which has two integers a and b: the numbers of coins in the piles.
-- Output
-- For each test, print "YES" if you can empty the piles and "NO" otherwise.
-- Constraints

-- 1 \le t \le 10^5
-- 0 \le a, b \le 10^9

-- Example
-- Input:
-- 3
-- 2 1
-- 2 2
-- 3 3

-- Output:
-- YES
-- NO
-- YES

-- A move removes three coins so to begin with you can only solve
-- piles where the total is a multiple of three.
-- And it's also impossible to solve if either pile is more than
-- twice the other pile, because of the ratio taken by each move (1:2).

piles :: Int -> Int -> Bool
piles a b = (a+b) `mod` 3 == 0 && a <= (b*2) && b <= (a*2)

result :: [Int] -> String
result [a,b] = if piles a b then "YES" else "NO"

solve :: Int -> IO ()
solve n = sequence_ (map (\_ -> getLine >>= (putStrLn . result . (map read) . words)) [1..n])

main :: IO ()
main = getLine >>= ( solve . read )

-- getLine reads a String into IO
-- read can make an `a` from the String (in our case, `a` needs to be Int)
-- call the resulting Int n
-- For n times, read another String, `words` can convert it into a two element list of String
--   map read over that list to get list of two Ints
--   call piles on the two Ints
--   print "YES" or "NO"
-- sequence putStrLn over the result list which wraps in IO for flatmap composition with getLine

