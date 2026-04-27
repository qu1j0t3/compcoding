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

-- https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html

-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Char.html
ansiReset = "\27[0m"
ansiRed   = "\27[31m"
ansiGreen = "\27[32m"
ansiCyan  = "\27[36m"
ansiBold  = "\27[1m"

red str     = ansiRed ++ str ++ ansiReset
cyan str    = ansiCyan ++ str ++ ansiReset
redBold str = ansiBold ++ ansiRed ++ str ++ ansiReset
green str   = ansiGreen ++ str ++ ansiReset

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
  If player1 plays                                         1 2 3 4 5 6 7 8
  and player 2 plays a 'left' rotation by n of this, e.g.  4 5 6 7 8 1 2 3
  then scores are distributed n to player 1                2 2 2 2 2 1 1 1
  Then we have to deal with corner cases:
  - if there is 1 card left, after playing all draws, then it can never score a point
    to a player, and so targets must be zero for the game to be valid
  - if there is more than one card left after playing draws, then neither target can be zero,
    because every play must score a point
-}

solve :: [Int] -> Maybe [(Int,Int)]
solve [cards, target1, target2] =
  let scoringPlays = target1 + target2
  in if scoringPlays <= cards
        && (target1 + target2 == 0 || (scoringPlays > 1 && target1 > 0 && target2 > 0))
     then Just $ [if a > scoringPlays then (a,a) else (a,1+((a-1-target2) `mod` scoringPlays)) | a <- [1..cards]]
     else Nothing


printSolution :: Maybe [(Int,Int)] -> IO ()
printSolution Nothing     = putStrLn "NO"
printSolution (Just game) = putStrLn "YES" >> p fst game >> p snd game
  where p f = putStrLn . unwords . (map (show . f))

failWithMessage :: String -> IO ()
failWithMessage err = hPutStrLn stderr (redBold err) >> exitWith (ExitFailure 1)

-- Given number of cards and target scores, check whether
-- the given plays reach the target scores.
-- return Nothing if plays given are not valid (each must be a permutation of cards 1..n)

checkGame :: Int -> Int -> Int -> [(Int,Int)] -> Maybe Bool
checkGame n t1 t2 game =
  if sort (map fst game) == [1..n] && sort (map snd game) == [1..n]
  then let scores = map (\(a,b) -> (a>b,b>a)) game
       in Just $ length (filter fst scores) == t1 && length (filter snd scores) == t2
  else Nothing


-- Run: ./main < 1.in --test 1.out
-- problems will be read from input and solved,
-- then checked against the solutions file given

--failedCheck str = putStrLn $ red str
failedCheck = failWithMessage

runWithChecking :: String -> IO ()
runWithChecking solutionFile =
  getLine >>= \ line1 ->
    withFile solutionFile ReadMode
      ( \ handle ->
          let gameCount::Int = read line1
              checkSolution :: [Int] -> Maybe [(Int,Int)] -> IO ()
              checkSolution [n, t1, t2] result =
                hGetLine handle >>= (\ expected ->
                    case result of
                      Nothing -> if expected == "NO"
                                 then putStrLn $ (green "  PASS") ++ " (" ++ expected ++ " is correct)"
                                 else failedCheck $ (red "FAIL:") ++ " no solution, but expected " ++ expected
                      Just game -> if expected == "YES"
                                   then -- skip two solution lines, because our solution is unlikely to match exactly.
                                        -- we have to verify this ourselves
                                     hGetLine handle >> hGetLine handle >>
                                       case checkGame n t1 t2 game of
                                         Just True  -> putStrLn $ (green "  PASS GAME: ") ++ (show game)
                                         Just False -> failedCheck "FAIL SCORES"
                                         Nothing    -> failedCheck "INVALID PLAYS"
                                   else failedCheck $ "FAIL: expected " ++ expected ++ " but got: " ++ (show game)
                  )
          in traverse_ (\ gameNumber -> getLine
                          >>= (\ l -> -- print input line for debugging
                            fmap (const l) (putStrLn $ "#" ++ (show gameNumber) ++ cyan (" Input: " ++ l)))
                          >>= ((\ input -> checkSolution input (solve input)) . (map read) . words))
                        [1..gameCount]
      )

main :: IO ()
main = getArgs >>= ( \case
            [] -> getLine >>= ( games . read )
            ["--check", solutionFile] -> runWithChecking solutionFile
            _ -> failWithMessage "run with no arguments, or --check solutionFile"
          )
  where games :: Int -> IO ()
        games n = traverse_ (\ _ -> getLine >>= (printSolution . solve . (map read) . words)) [1..n]
