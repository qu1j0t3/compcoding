-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Raab Game I
-- https://cses.fi/problemset/task/3399

-- Consider a two player game where each player has n cards numbered 1,2,\dots,n. On each turn both players place one of their cards on the table. The player who placed the higher card gets one point. If the cards are equal, neither player gets a point. The game continues until all cards have been played.
-- You are given the number of cards n and the players' scores at the end of the game, a and b. Your task is to give an example of how the game could have played out.
-- Input
-- The first line contains one integer t: the number of tests.
-- Then there are t lines, each with three integers n, a and b.
-- Output
-- For each test case print YES if there is a game with the given outcome and NO otherwise.
-- If the answer is YES, print an example of one possible game. Print two lines representing the order in which the players place their cards. You can give any valid example.
-- Constraints

-- 1 \le t \le 1000
-- 1 \le n \le 100
-- 0 \le a,b \le n

-- Example
-- Input:
-- 5
-- 4 1 2
-- 2 0 1
-- 3 0 0
-- 2 1 1
-- 4 4 1

-- Output:
-- YES
-- 1 4 3 2
-- 2 1 3 4
-- NO
-- YES
-- 1 2 3
-- 1 2 3
-- YES
-- 1 2
-- 2 1
-- NO

{-
A game is a zip'd pair of permutations of respective players' cards
we need to find a game with: exact number of a>b, exact number of b>a, and rest are all draws

i guess we can do this with backtracking search?

-}

import Data.Foldable
import Data.List
import Data.Maybe
-- import Data.IntSet (IntSet, fromList)
-- import qualified Data.IntSet as IntSet


-- permgen :: Eq a => [a] -> [[a]]
-- permgen [] = [[]]
-- permgen xs = concatMap (\ x -> map (x:) (permgen (delete x xs))) xs

-- A game is a permutation of one hand
-- played against 1..n

-- This takes a search approach but it's too slow for the size of problem in CSES judge; breaks down around 9 cards
solve :: [Int] -> Maybe [(Int,Int)]
solve [n, target1, target2] =
  let draws = n - target1 - target2
      n' = n - draws
      drawPlays = [(a,a) | a <- [n'+1 .. n]]
  in if draws >= 0 && target1 < n' && target2 < n'
     then fmap (drawPlays ++) $
            step [1..n'] [1..n'] n' target1 target2 [] -- search possible plays, each player must meet target with cards 1..n'
     else Nothing
     where step :: [Int] -> [Int] -> Int -> Int -> Int -> [(Int,Int)] -> Maybe [(Int,Int)]
           step _ _ 0 0 0 acc = Just acc -- success! this game is a solution
           step p1 p2 n t1 t2 acc =
             listToMaybe $ -- we only want one complete game
               catMaybes [ if a > b then step (delete a p1) (delete b p2) (n-1) (t1-1) t2 ((a,b):acc)
                                    else step (delete a p1) (delete b p2) (n-1) t1 (t2-1) ((a,b):acc)
                           | a <- p1, b <- p2, a /= b ]

  -- step [1..n] [1..n] 0 0
  -- where
  --   step :: [Int] -> [Int] -> Int -> Int -> [[(Int,Int)]]
  --   -- solve subproblem given hands for players, target scores
  --   step h1 h2 t1 t2 =
  --     let result if t1 < target1 then

    -- step [] [] t1 t2 = []
    -- step (a:h1) (b:h2) t1 t2 | a > b && t1 < target1 = (a,b) : step h1 h2 (t1+1) t2
    --                          | b > a && t2 < target2 = (a,b) : step h1 h2 t1 (t2+1)
    --                          | a == b                = (a,b) : step h1 h2 t1 t2 -- we could try guard t1==target1 && t2==target2
    --                          | otherwise             = []

printSolution :: Maybe [(Int,Int)] -> IO ()
printSolution Nothing     = putStrLn "NO"
printSolution (Just game) = putStrLn "YES" >> p fst game >> p snd game
  where p f = putStrLn . unwords . (map (show . f))

main :: IO ()
main = getLine >>= ( games . read )
  where games :: Int -> IO ()
        games n = traverse_ (\ _ -> getLine >>= (printSolution . solve . (map read) . words)) [1..n]
