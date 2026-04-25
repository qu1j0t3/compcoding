-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Raab Game I
-- https://cses.fi/problemset/task/3399

{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.Exit
import System.IO

import Data.Foldable
import Data.List
-- import Data.Maybe
-- import Data.IntSet (IntSet, fromList)
-- import qualified Data.IntSet as IntSet

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


-- permgen :: Eq a => [a] -> [[a]]
-- permgen [] = [[]]
-- permgen xs = concatMap (\ x -> map (x:) (permgen (delete x xs))) xs


solve :: [Int] -> Maybe [(Int,Int)]
solve [cards, target1, target2] =
  if cards >= target1 + target2
  then step (reverse [1..cards]) (reverse [1..cards]) cards target1 target2 []
  else Nothing
    where step :: [Int] -> [Int] -> Int -> Int -> Int -> [(Int,Int)] -> Maybe [(Int,Int)]
          step _  _ 0 0 0 acc = Just acc -- success! this game is a solution
          step [] _ _ _ _ _   = Nothing -- can't exhaust either hand with non zero targets
          step _ [] _ _ _ _   = Nothing
          step p1@(h1:rest1) p2@(h2:rest2) n t1 t2 acc
            | n > target1 + target2 = -- play all draws first; we assume h1 and h2 are equal by construction above
              step rest1 rest2 (n-1) t1 t2 ((h1,h2):acc)
            | t1 > 0    = -- if either t1 > 0 or t2 > 0, we're not finished playing
              find (<h1) p2 >>= \ lose2 -> step rest1 (delete lose2 p2) (n-1) (t1-1) t2 ((h1,lose2):acc)
            | otherwise =
              find (<h2) p1 >>= \ lose1 -> step (delete lose1 p1) rest2 (n-1) t1 (t2-1) ((lose1,h2):acc)

printSolution :: Maybe [(Int,Int)] -> IO ()
printSolution Nothing     = putStrLn "NO"
printSolution (Just game) = putStrLn "YES" >> p fst game >> p snd game
  where p f = putStrLn . unwords . (map (show . f))

failWithMessage :: String -> IO ()
failWithMessage err = hPutStrLn stderr err >> exitWith (ExitFailure 1)

-- Given number of cards and target scores, check whether
-- the given plays are a valid solution

checkGame :: [Int] -> [(Int,Int)] -> Maybe Bool
checkGame [n, t1, t2] game =
  if sort (map fst game) == [1..n] && sort (map snd game) == [1..n]
  then let scores = map (\(a,b) -> (a>b, a<b)) game
         in Just $ length (filter fst scores) == t1 && length (filter snd scores) == t2
  else Nothing

-- Run: ./main < 1.in --test 1.out
-- problems will be read from input and solved,
-- then checked against the solutions file given

runWithChecking :: String -> IO ()
runWithChecking solutionFile =
  getLine >>= \ line1 ->
    withFile solutionFile ReadMode
      ( \ handle ->
          let n::Int = read line1
              checkSolution :: [Int] -> Maybe [(Int,Int)] -> IO ()
              checkSolution input result =
                hGetLine handle >>= (\ expected ->
                    case result of
                      Nothing -> if expected == "NO"
                                 then putStrLn "  PASS"
                                 else failWithMessage $ "FAIL: no solution, but expected " ++ expected
                      Just game -> if expected == "YES"
                                   then -- skip two solution lines, as we will check ourselves
                                     hGetLine handle >> hGetLine handle >>
                                       case checkGame input game of
                                         Nothing    -> failWithMessage "INVALID PLAYS"
                                         Just True  -> putStrLn $ "  PASS GAME: " ++ (show game)
                                         Just False -> failWithMessage "FAIL SCORES"
                                         -- hPutStrLn stderr sol1 >> hPutStrLn stderr sol2
                                   else failWithMessage $ "FAIL: expected " ++ expected
                  )
          in traverse_ (\ gameNumber -> getLine
                          >>= (\ l -> fmap (const l) (putStrLn $ "#" ++ (show gameNumber) ++ " Input: " ++ l)) -- print input lines for debugging
                          >>= ((\ input -> checkSolution input (solve input)) . (map read) . words))
                        [1..n]
      )

main :: IO ()
main = getArgs >>= ( \case
            ["--test", solutionFile] -> runWithChecking solutionFile
            [] -> getLine >>= ( games . read )
            _ -> failWithMessage "run with no arguments, or --test solutionFile"
          )
  where games :: Int -> IO ()
        games n = traverse_ (\ _ -> getLine >>= (printSolution . solve . (map read) . words)) [1..n]
