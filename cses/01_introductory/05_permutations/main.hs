-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Permutations
-- https://cses.fi/problemset/task/1070


perm :: Int -> String

perm count = let half = (count + 1) `div` 2
                 left = take half [1,3..]
                 lastItem = 2*half-1 -- the last item of `left`
                 -- if lastItem is less than 4, then the joined lists
                 -- will have a difference of < 2, making a non-beautiful permutation
                 right = take (count - half) [2,4..]
             in if lastItem < 4
                then "NO SOLUTION"
                else unwords (map show (left ++ right))

main :: IO ()

main = getLine >>= (putStrLn . perm . read)

