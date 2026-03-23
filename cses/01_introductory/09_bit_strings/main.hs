-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Bit Strings
-- https://cses.fi/problemset/task/1617



strings :: Integer -> Integer

strings n = (2^n) `mod` (10^9 + 7)


main :: IO ()

main = getLine >>= ( print . strings . read )


