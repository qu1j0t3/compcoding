-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Palindrome Reorder
-- https://cses.fi/problemset/task/1755

-- Given a string, your task is to reorder its letters in such a way
-- that it becomes a palindrome (i.e., it reads the same forwards and backwards).
-- Input
-- The only input line has a string of length n consisting of characters A–Z.
-- Output
-- Print a palindrome consisting of the characters of the original string.
-- You may print any valid solution. If there are no solutions, print "NO SOLUTION".
-- Constraints

-- 1 \le n \le 10^6

-- Example
-- Input:
-- AAAACACBA

-- Output:
-- AACABACAA

import Data.Map.Lazy (Map, insertLookupWithKey, foldrWithKey)
import qualified Data.Map.Lazy as Map

countChar :: Char -> Map Char Int -> Map Char Int
countChar c m = snd (insertLookupWithKey (\_ _ oldv -> oldv+1) c 1 m)

accumChar :: Char -> Int -> String -> String
accumChar c n acc = if even n then (take (n `div` 2) (repeat c)) ++ acc else acc

solve :: String -> String
solve input = let histogram = foldr countChar Map.empty input
                  odds = Map.filter odd histogram
                  half = foldrWithKey accumChar "" histogram
                  oddString = foldrWithKey (\c n _ -> take n (repeat c)) "" odds
              in if length odds > 1 then "NO SOLUTION" else half ++ oddString ++ (reverse half)

main :: IO ()
main = getLine >>= ( putStrLn . solve )
