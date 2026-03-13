-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Two Knights
-- https://cses.fi/problemset/task/1072


board :: Int -> [Int]
board maxk = map moves [1..maxk]

-- Ways of placing two knights with no other constraints

two_knights :: Int -> Int
two_knights k = k*k * (k*k-1) `div` 2

-- (Hint from traxex)
-- number of ways of arranging a 2x3 piece into a kxk board

arrangements_2x3 :: Int -> Int
arrangements_2x3 k = 2*(k-1)*(max (k-2) 0)

-- A 2x3 piece has TWO mutually threatening
-- knight configurations inside it
-- so this value helps us compute how many arrangements
-- to subtract from two_knights k.

moves :: Int -> Int

moves 1 = 0
moves k = two_knights k - 2*(arrangements_2x3 k)

-- Input:
-- 8

-- Output:
-- 0
-- 6
-- 28
-- 96
-- 252
-- 550
-- 1056
-- 1848


main :: IO ()

main = getLine >>= (sequence_ . (map (putStrLn . show)) . board . read)


