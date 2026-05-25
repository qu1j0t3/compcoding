-- DIGRESSION
-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Knapsack Problems, page 83, Competitive Programmer’s Handbook
-- * https://cses.fi/book/book.pdf

-- Given a list of weights [w1,w2,...,wn], determine all sums
-- that can be constructed using the weights.

-- import Data.IntSet (IntSet)
-- import qualified Data.IntSet as IntSet

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

import System.Environment (getArgs)
import System.Exit
import System.IO

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

-- instance Show Item where
--   showsPrec _ (Item w v) s = show w ++ "," ++ show v ++ s


-- Naive recursive version (exponential complexity)
--   at each step, given remaining items,
--   return maximum value achieved by either selecting, or not selecting each item
--   while not exceeding knapsack capacity.

sumWeight :: [Item] -> Int
sumWeight items = sum $ map weight items

sumValues :: [Item] -> Int
sumValues items = sum $ map value items

greaterValue :: [Item] -> [Item] -> [Item]
greaterValue ls rs = if sumValues ls > sumValues rs then ls else rs

printItem :: Item -> String
printItem (Item w v) = "  (weight: " ++ show w ++ " value: " ++ show v ++ ")"

solve :: Int -> [Item] -> [Item]
solve capacity items = step items []
  where step :: [Item] -> [Item] -> [Item]
        step [] carrying = carrying
        step (i:rest) carrying | weight i + sumWeight carrying > capacity = step rest carrying
                               | otherwise = greaterValue (step rest (i:carrying)) (step rest carrying)

-- e.g.   cabal run knapsack-dp 6  1,10 3,40 2,15 1,15

main :: IO ()
main = getArgs >>= (
    \case (capacity:items) ->
            let cap = read capacity
                sol = solve cap (map read items)
            in putStrLn ("Capacity: " ++ show cap)
               >> putStrLn "Carry:"
               >> mapM_ (putStrLn . printItem) sol
               >> putStrLn ("Value: " ++ show (sum $ map value sol))
          _ -> hPutStrLn stderr "args: capacity weight1,value1 weight2,value2 ..."
               >> exitWith (ExitFailure 1)
  )
