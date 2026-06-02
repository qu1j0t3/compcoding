-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Knapsack Problems, page 83, Competitive Programmer’s Handbook
-- * https://cses.fi/book/book.pdf

-- Given a list of weights [w1,w2,...,wn], determine all sums
-- that can be constructed using the weights.


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random

-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

-- import Data.Set (Set)
-- import qualified Data.Set as Set

import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.List.Split




-- Inputs:  capacity of knapsack; list of n item values

-- Problem: work out the selection of items (among 2^n different selections)
--          that maximises the value packed into knapsack
--          without exceeding capacity


data Item = Item {weight,value::Int}

instance Read Item where
  readsPrec :: Int -> ReadS Item
  readsPrec _ s = case splitOn "," s of
                    [w,v] -> [(Item (read w) (read v), "")]
                    _ -> []

instance Show Item where
  showsPrec _ (Item w v) s = show w ++ "," ++ show v ++ s


-- Naive recursive version (exponential complexity)
--   at each step, given remaining items,
--   return maximum value achieved by either selecting, or not selecting each item
--   while not exceeding knapsack capacity.

greaterValue :: [Item] -> [Item] -> [Item]
greaterValue ls rs = if sumValues ls > sumValues rs then ls else rs
  where sumValues items = sum $ map value items

printItem :: Item -> String
printItem (Item w v) = "  (weight: " ++ show w
                       ++ " value: " ++ show v
                       ++ " density: " ++ show (fromIntegral v / fromIntegral w) ++ ")"

solve :: [Item] -> Int -> [Item]
solve items capacity  = step items capacity []
  where step :: [Item] ->  Int -> [Item] -> [Item]
        step [] _ carrying = carrying
        step (i:rest) cap carrying | weight i > cap = step rest cap carrying
                                   | otherwise = greaterValue (step rest (cap - weight i) (i:carrying)) (step rest cap carrying)

-- e.g.   cabal run knapsack-dp 6  1,10 3,40 2,15 1,15

solvePrint :: Int -> [Item] -> IO ()
solvePrint cap items =
  let sol = solve items cap
      m = makeMap items cap
  in putStrLn ("Capacity: " ++ show cap)
      >> putStrLn "Carry:"
      >> mapM_ (putStrLn . printItem) sol
      >> putStrLn ("Weight: " ++ show (sum $ map weight sol))
      >> putStrLn ("Value: " ++ show (sum $ map value sol))
      >> printMap m cap (length items)

randItems :: [Int] -> Int -> [Item]
randItems (w:k:k2:rest) cap =
  Item wt (density*wt) : randItems rest cap
  where wt = 1 + (abs w `mod` cap)
        density = 2 + ceiling (log (fromIntegral (abs k) / fromIntegral (abs k2)))


-- "Tabulation" solver - e.g. Abdul Bari https://youtu.be/nLmhmB6NzcM
-- ~ memoisation where the subproblem index (i,c) is maximum value possible in capacity c after item i
-- where items are numbered arbitrarily

-- The map is an immutable data structure that we build
-- by rows, from i=1 up to i=n (row i=0 is all zeroes).
-- Each row is indexed from capacity 1 to capacity c (max).
-- Column capacity 0 is all zero.
-- To compute each cell we look at previous row only,
-- which is why we build the map in order from row i = 1 to n
makeMap :: [Item] -> Int -> Map (Int,Int) Int
makeMap items capacity =
  -- The direction of fold here is very important. It must be FOLD LEFT because
  -- we want to update the map as we consume, left-to-right, the given list of row indices (1..n)
  foldl' (\ m' (i,item) ->
          foldl' (\ m'' c -> let lkp 0 = 0  -- with zero capacity, value is always zero
                                 lkp c' = if i > 1 then fromJust $ Map.lookup (i-1,c') m'' else 0
                                 withoutItem = lkp c  -- max value attainable for capacity c, with items 1..i-1
                                  -- considering item i, it can only be taken if capacity c fits the item
                                  -- if it's TAKEN, and weight w, then we need to calculate max value for previous items,
                                  --                but under a capacity reduced by w. So, index by that reduced capacity
                                 withItem = value item + lkp (c - weight item)
                                 maxValue = if c >= weight item then max withItem withoutItem else withoutItem
                             in Map.insert (i,c) maxValue m'')
                 m'
                 [1..capacity])
         Map.empty
         (zip [1..] items)

printMap :: Map (Int,Int) Int -> Int -> Int -> IO ()
printMap m cap n =
  mapM_ (putStrLn . unwords)
        [ map (\c -> maybe "---" (\ v -> let s = show v in replicate (3 - length s) ' ' ++ s) (Map.lookup (i,c) m))
              [1..cap]
          | i <- [1..n] ]

-- benchmark:  cabal run knapsack-dp -- --seed 14302 100 100     67.34s user

main :: IO ()
main = getArgs >>= (
    \case
      -- run the naive solver
      ["--random", capacity, n] -> newStdGen >>= genSolve (read capacity) (read n)

      ["--seed", seed, capacity, n] -> genSolve (read capacity) (read n) (mkStdGen (read seed))

      -- run the memoised solver
      ["--seedM", seed, capacity, n] -> genSolveM (read capacity) (read n) (mkStdGen (read seed))

      -- This will eat any set of one or more arguments regardless of whether they parse at runtime (may crash)
      -- FIXME: validate with `reads` so that we fall through to usage message if any argument fails to parse
      (capacity:items) -> solvePrint (read capacity) (map read items)

      _ -> hPutStrLn stderr "args: capacity weight1,value1 weight2,value2 ..."
        >> hPutStrLn stderr "  or: --random capacity number_of_items       to solve a random problem"
        >> hPutStrLn stderr "  or: --seed seed capacity number_of_items    to solve a random problem with specified random seed"
        >> hPutStrLn stderr "  or: --seedM seed capacity number_of_items   to solve same problem with memoised solver (fast)"
        >> exitWith (ExitFailure 1)
  )
  where genSolve :: Int -> Int -> StdGen -> IO ()
        genSolve cap n gen =
          let items = take n (randItems (randoms gen :: [Int]) cap)
          in solvePrint cap items

        genSolveM :: Int -> Int -> StdGen -> IO ()
        genSolveM cap n gen =
          let items = take n (randItems (randoms gen :: [Int]) cap)
              m = makeMap items cap
          in putStrLn ("Max value: " ++ show (Map.lookup (n,cap) m))

