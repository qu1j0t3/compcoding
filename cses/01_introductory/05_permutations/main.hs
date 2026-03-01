-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Permutations
-- https://cses.fi/problemset/task/1070


perm :: Int -> String

perm 1 = "1"
perm count | count < 4 = "NO SOLUTION"
perm count = let half = count `div` 2
                 left = take half [2,4..]
                 right = take (count - half) [1,3..]
             in unwords (map show (left ++ right))

main :: IO ()

main = getLine >>= (putStrLn . perm . read)

