-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Factorial
-- https://cses.fi/problemset/task/1618

-- Your task is to calculate the number of trailing zeros in the factorial n!.
-- For example, 20!=2432902008176640000 and it has 4 trailing zeros.
-- Input
-- The only input line has an integer n.
-- Output
-- Print the number of trailing zeros in n!.
-- Constraints

-- 1 \le n \le 10^9

-- Example
-- Input:
-- 20

-- Output:
-- 4


-- I think the number of trailing zeroes
-- is equal to the power of 5 in the prime factorisation
-- because there are always more 2's in the prime factorisation
-- and every 2,5 pair means a x 10 factor (i.e. a trailing zero).

-- Repeated division is too slow for the judge
-- powerOfFive :: Integer -> Integer
-- powerOfFive n = if n `mod` 5 == 0 then 1 + powerOfFive (n `div` 5) else 0

-- This means that we can't compute the factorial itself, I guess
-- extracting powers of five from the input, works, up to 25 which counts as one but needs to count as two,
-- and then beyond this?
--         5          5              5              5             5x5
-- 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

-- so how about this,
-- we divide by 5; if < 5, that's it, otherwise add q to the result of recursing on the quotient
-- this is very fast since we're not handling any numbers larger than n

f :: Integer -> Integer
f n = let q = n `div` 5 in if q < 5 then q else q + f q


main :: IO ()

main = getLine >>= ( print . f . read )


