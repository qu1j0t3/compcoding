-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

{-# LANGUAGE LambdaCase #-}

-- import System.Environment
-- import System.Exit
-- import System.IO

-- import Data.Foldable
-- import Data.List
-- import Data.Bits

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

import Data.IntSet (IntSet, fromList)
import qualified Data.IntSet as IntSet


-- Solution to CSES Introductory Problems Knight Moves Grid
-- https://cses.fi/problemset/task/3217

-- There is a knight on an n \times n chessboard. For each square, print the minimum number of moves the knight needs to do to reach the top-left corner.
-- Input
-- The only line has an integer n.
-- Output
-- Print the number of moves for each square.
-- Constraints

-- 4 \le n \le 1000

-- Example
-- Input:
-- 8

-- Output:
-- 0 3 2 3 2 3 4 5
-- 3 4 1 2 3 4 3 4
-- 2 1 4 3 2 3 4 5
-- 3 2 3 2 3 4 3 4
-- 2 3 2 3 4 3 4 5
-- 3 4 3 4 3 4 5 4
-- 4 3 4 3 4 5 4 5
-- 5 4 5 4 5 4 5 6

-- Haha https://oeis.org/A018837
-- & https://oeis.org/A183043


-- | Naive recursive solution
-- Compiled with ghc, this is excessively slow when n=6
moves :: Int -> Int -> Int -> Set (Int,Int) -> Maybe Int
moves _ 1 1 _   = Just 0
moves n i j acc =
  case catMaybes [ (1+) <$> moves n (i+a) (j+b) (Set.insert (i+a,j+b) acc)
                   | a <- [-2..2], b <- [-2..2], abs a + abs b == 3,
                     i+a >= 1, i+a <= n, j+b >= 1, j+b <= n, Set.notMember (i+a,j+b) acc ] of
    [] -> Nothing
    xs -> Just $ minimum xs


solve :: Int -> [[Maybe Int]]
solve n = [[moves n i j (Set.singleton (i,j)) | j <- [1..n]] | i <- [1..n]]


-- memoised version
-- excessively slow when n=17

type SolutionMap = Map (Int,Int) Int

-- | The eight valid moves of a chess knight, starting from (0,0)
knightMoves :: [(Int,Int)]
knightMoves = [ (a,b) | a <- [-2..2], b <- [-2..2], abs a + abs b == 3 ]

-- Count knight moves from given square i,j in order to reach origin 1,1
-- (the base case: always 0 moves).
-- At each square i,j that is not the origin,
-- we take the smallest solution from each of the valid 8 knight moves from that square,
-- and add one. we then update the solution map for i,j (memoisation).
-- to prevent loops, we avoid re-evaluating any square already visited in this path.

-- | Compute minimum number of knight moves from square (i,j) to origin (1,1)
-- Memoisation is used to make this more efficient.
movesM :: SolutionMap -- ^ solutions computed so far, as a map from position to move count
       -> Int         -- ^ size of board
       -> Int         -- ^ row index i
       -> Int         -- ^ column index j
       -> IntSet      -- ^ set of squares already visited in this evaluation, as a set of integer labels
       -> (Maybe Int,SolutionMap) -- ^ return count (if possible from given square) and updated map
movesM m _ 1 1 _       = (Just 0,m)
movesM m n i j visited | i<1 || j<1 || i>n || j>n || IntSet.member (i*n+j) visited = (Nothing,m)
                       | otherwise =
  case Map.lookup (i,j) m of
    Nothing ->
      -- This is the recursive case:
      -- count moves from (i,j) to (1,1) by taking minimum of all possible paths
      -- IS THE BUG THAT WE CAN ADD TO VISITED SET
      -- YET NOT SET AN ENTRY IN THE MAP?
      let visited' = IntSet.insert (i*n+j) visited
          -- use a fold to accumulate state (solution map)
          -- over all new directions
          (paths,m') = foldr (\  (a,b) (cs,mm) ->
                                      case movesM mm n (i+a) (j+b) visited' of
                                        (Just c,newMap)  -> (c:cs,newMap)
                                        (Nothing,newMap) -> (cs,newMap))
                              ([],m)
                              knightMoves
      in case paths of
        [] -> (Nothing,m')
        ps -> let s = 1 + minimum ps in (Just s, Map.insert (i,j) s m')
    sol -> (sol,m) -- return cached solution at (i,j)

-- solveM :: Int -> [[Maybe Int]]
-- solveM n = [[fst $ movesM Map.empty n i j IntSet.empty | j <- [1..n]] | i <- [1..n]]

-- | Produce 2D solution for board of size n
solveM :: Int -> [[Maybe Int]]
solveM n = let m = foldr (\ i m' ->
                            foldr (\ j m'' -> snd $ movesM m'' n i j IntSet.empty)
                                  m'
                                  [1..n])
                         Map.empty
                         [1..n]
           in [ [fst $ movesM m n i j IntSet.empty | j <- [1..n]] | i <- [1..n] ]


main :: IO ()
main = getLine >>= (\ input -> mapM_ (putStrLn . unwords . map printSquare) (solveM $ read input))
  where printSquare (Just c) = show c  -- even though the problem doesn't call for it,
        printSquare Nothing  = "-"     -- this allows us to correctly print boards smaller than 4x4
