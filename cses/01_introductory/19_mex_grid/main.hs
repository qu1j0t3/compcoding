-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

{-# LANGUAGE LambdaCase #-}

-- import System.Environment
-- import System.Exit
-- import System.IO

-- import Data.Foldable
-- import Data.List
import Data.Bits
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- import Data.Maybe
-- import Data.IntSet (IntSet, fromList)
-- import qualified Data.IntSet as IntSet


-- Solution to CSES Introductory Problems Mex Grid Construction
-- https://cses.fi/problemset/task/3419

-- Interesting... https://oeis.org/A003987
-- https://oeis.org/A004442 Nimsum n+1
-- https://oeis.org/A004443 Nimsum n+2

-- Your task is to construct an n \times n grid where each square has the smallest nonnegative integer that does not appear to the left on the same row or above on the same column.
-- Input
-- The only line has an integer n.
-- Output
-- Print the grid according to the example.
-- Constraints

-- 1 \le n \le 100

-- Example
-- Input:
-- 5

-- Output:
-- 0 1 2 3 4
-- 1 0 3 2 5
-- 2 3 0 1 6
-- 3 2 1 0 7
-- 4 5 6 7 0


-- In ghci, this starts to get pretty show around order 11

square :: Int -> Int -> Int
square 0 j = j
square i 0 = i
square i j | i == j = 0
           | i > j = square j i -- solution is symmetric across diagonal
           | otherwise =  -- i < j
              let filled = [square i' j | i' <- [0..i-1]] ++ [square i j' | j' <- [0..j-1]]
                  v = head (filter (\ e -> notElem e filled) [1..])
              in v

solve :: Int -> [[Int]]
solve n = [[square i j | j <- [0..n-1]] | i <- [0..n-1]]


type SolutionMap = Map (Int,Int) Int

-- |Memoising evaluator. If solution is in map, return it, and unchanged map.
-- |otherwise, call self for subproblems and return solution with updated map.
squareM :: SolutionMap -> Int -> Int -> (Int,SolutionMap)
squareM m 0 j = (j,m)
squareM m i 0 = (i,m)
squareM m i j | i == j = (0,m)
              | i > j = squareM m j i
              | otherwise =
                  case Map.lookup (i,j) m of
                    Just v -> (v,m)
                    Nothing -> let (as,m')  = foldr (\ i' (acc,mm) -> case squareM mm i' j of (v,updated) -> (v:acc,updated))
                                                    ([],m) (reverse [0..i-1])
                                   (bs,m'') = foldr (\ j' (acc,mm) -> case squareM mm i j' of (v,updated) -> (v:acc,updated))
                                                    (as,m') (reverse [0..j-1])
                                   sol = head $ filter ((flip notElem) bs) [1..]
                               in (sol, Map.insert (i,j) sol m'')

solveM :: Int -> [[Int]]
solveM n = let m = snd $ squareM Map.empty n n -- populate solution map
           in [[fst $ squareM m i j | j <- [0..n-1]] | i <- [0..n-1]]


-- OK, after doing this the dumb and slow way
-- I noticed by pasting some rows of the matrix into OEIS
-- that they are Nimsums, XORs of row and column.
-- Nicely played, CSES...

solveXor :: Int -> [[Int]]
solveXor n = [[i `xor` j | j <- [0..n-1]] | i <- [0..n-1]]



main :: IO ()
main = getLine >>= (\ input -> sequence_ $ fmap putStrLn (map (\r -> unwords $ map show r) (solveXor $ read input)))
