-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Creating Strings
-- https://cses.fi/problemset/task/1622

-- Given a string, your task is to generate all different strings that can be created using its characters.
-- Input
-- The only input line has a string of length n. Each character is between a–z.
-- Output
-- First print an integer k: the number of strings. Then print k lines: the strings in alphabetical order.
-- Constraints

-- 1 \le n \le 8

-- Example
-- Input:
-- aabac

-- Output:
-- 20
-- aaabc
-- aaacb
-- ...

-- just cheat and use permutations because I know how to do permutations!
-- permutations of empty string: [[]]
-- (assume input is sorted; take each element; permutations are this element
--  prefixing every permutations of the string with that element removed)
-- input: RED   sorted: EDR
-- perms(EDR): E*perms(DR) ++ D*perms(ER) ++ R*perms(ED)
-- perms(DR): D*perms(R) ++ R*perms(D) = DR,RD ...
-- or:
--   ghci> sort $ permutations "RED"
--   ["DER","DRE","EDR","ERD","RDE","RED"]

import Data.List
import Data.Set (toAscList, fromList)

-- The problem asks for unique permutations, in order, so convert
-- to a Set then produce a list from it, in ascending order.

solve :: String -> [String]
solve s = toAscList . fromList $ permutations s

main :: IO ()
main = getLine >>= (sequence_ . (\ps -> (print (length ps)):(map putStrLn ps)) . solve)

