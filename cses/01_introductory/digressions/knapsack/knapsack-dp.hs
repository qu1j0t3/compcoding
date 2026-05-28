-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Knapsack Problems, page 83, Competitive Programmer’s Handbook
-- * https://cses.fi/book/book.pdf

-- Given a list of weights [w1,w2,...,wn], determine all sums
-- that can be constructed using the weights.


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

-- import Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap

-- import Data.Set (Set)
-- import qualified Data.Set as Set

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random

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
printItem (Item w v) = "  (weight: " ++ show w ++ " value: " ++ show v ++ " density: " ++ show (fromIntegral v / fromIntegral w) ++ ")"

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
  in putStrLn ("Capacity: " ++ show cap)
      >> putStrLn "Carry:"
      >> mapM_ (putStrLn . printItem) sol
      >> putStrLn ("Weight: " ++ show (sum $ map weight sol))
      >> putStrLn ("Value: " ++ show (sum $ map value sol))

randItems :: [Int] -> Int -> [Item]
randItems (w:k:k2:rest) cap =
  Item wt (density*wt) : randItems rest cap
  where wt = 1 + (abs w `mod` cap)
        density = 2 + ceiling (log (fromIntegral (abs k) / fromIntegral (abs k2)))

main :: IO ()
main = getArgs >>= (
    \case
      ["--random", capacity, n] ->
        newStdGen >>= \ gen ->
          let cap = read capacity
              items = take (read n) (randItems (randoms gen :: [Int]) cap)
          in solvePrint cap items
      -- This will eat any set of one or more arguments regardless of whether they parse at runtime (may crash)
      -- FIXME: validate with `reads` so that we fall through to usage message if any argument fails to parse
      (capacity:items) -> solvePrint (read capacity) (map read items)
      _ -> hPutStrLn stderr "args: capacity weight1,value1 weight2,value2 ..."
        >> hPutStrLn stderr "  or: --random capacity number_of_items       to solve a random problem"
        >> exitWith (ExitFailure 1)
  )
