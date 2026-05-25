-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- References:
-- * Haddock markup https://haskell-haddock.readthedocs.io/latest/markup.html#
-- * Style https://wiki.haskell.org/Programming_guidelines#Good_Programming_Practice

-- Knapsack Problems, page 83, Competitive Programmer’s Handbook
-- * https://cses.fi/book/book.pdf

-- Given a list of weights [w1,w2,...,wn], determine all sums
-- that can be constructed using the weights.

-- import Data.IntSet (IntSet)
-- import qualified Data.IntSet as IntSet

import System.Environment (getArgs)

-- |Can the given weight be constructed by combining weights from a list
possible :: [Int]   -- ^the list of usable weights
            -> Int  -- ^the target weight
            -> Bool
possible [] 0 = True -- can always make a zero weight from no weights
possible [] _ = False -- cannot make a positive or negative weight from no weights
-- the simplistic recursive function that costs
-- exponential time because of the branching structure
possible (w:ws) x =
  possible ws x -- can it be done with remaining weights if we ignore next one (w)
  || possible ws (x-w) -- can it be done with remaining weights AND w


-- |Returns printable version of the `possible` result
ch :: Bool -> Char
ch False = ' '
ch True  = 'X'

solve :: [Int] -> (Int,[Char])
solve ws = (sum ws, map (ch . possible ws) [0..sum ws])

main :: IO ()
main = getArgs >>= (print . solve . map read)
