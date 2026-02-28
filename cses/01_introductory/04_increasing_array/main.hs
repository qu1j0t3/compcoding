-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Increasing Array
-- https://cses.fi/problemset/task/1094

-- import Data.List


-- Proceed from left to right, maintaining the count
-- of change steps needed and the current highest level
-- reached. For each value, if it's less than the current
-- level, add the difference to the accumulated steps,
-- otherwise adjust the current level to the new value.

sumGaps :: [Int] -> Int

sumGaps (x:xs) =
  fst (foldl (\(acc,level) v ->
         (acc+(max 0 (level-v)),max level v)) (0,x) xs)
sumGaps [] = 0


increasing :: Int -> IO Int

increasing count = fmap (sumGaps . (map read) . (take count) . words) getLine


main :: IO ()

main = getLine >>= (increasing . read) >>= print

