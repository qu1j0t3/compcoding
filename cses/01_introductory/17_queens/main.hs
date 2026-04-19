-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Chessboard and Queens
-- https://cses.fi/problemset/task/1624

-- Your task is to place eight queens on a chessboard so that no two queens are attacking each other. As an additional challenge, each square is either free or reserved, and you can only place queens on the free squares. However, the reserved squares do not prevent queens from attacking each other.

-- How many possible ways are there to place the queens?

-- Input
-- The input has eight lines, and each of them has eight characters. Each square is either free (.) or reserved (*).

-- Output
-- Print one integer: the number of ways you can place the queens.

-- Example
-- Input:

-- ........
-- ........
-- ..*.....
-- ........
-- ........
-- .....**.
-- ...*....
-- ........
-- Output:

-- 65

-- |Given lists of reserved columns for each row,
-- |compute how many non-attacking queens can be placed on 8x8 board.

solve :: [[Int]] -> Int
solve input =
  step [] input 8
  where
    step :: [(Int,Int)] -> [[Int]] -> Int -> Int
    step _ _ 0 = 1 -- base case: placed all rows, so count one solution
    step placed blocked r =
      -- place Q in each possible column of row r and return sum of each resulting subproblem (r-1)
      let rowBlocked = blocked !! (r-1)
      in sum [ let updatedBlocked = -- update unplaced rows (< r) with new threatened columns
                    map (\ (r',cs) -> let d = abs (r-r')
                                      in c:(c+d):(c-d):cs)
                        (zip [1..] (take (r-1) blocked))
               in step ((r,c):placed) updatedBlocked (r-1)
               | c <- [1..8], not (elem c rowBlocked) ]

--  sum [ if reserved!(i,j) then 1 else 0 | i <- [1..8], j <- [1..8] ] -- just count reserved squares

-- We don't use this, but kept as reference
--
-- readInputAssocList :: IO [((Int,Int),Bool)]
-- readInputAssocList =
--   fmap mconcat -- flatten the 2-D input into a list of index/value tuples
--        (traverse (\ r -> -- traverse over 8 rows
--                  fmap (\ line -> map (\ (c,b) -> ((r,c),b)) -- pair (row,col) tuples with reserved flags
--                                      (zip [1..] (map (== '*') line))) -- add column index and test for '*'
--                       getLine) -- read each row as a line (string)
--                  [1..8])

readInput :: IO [[Int]]
readInput =
  traverse (\ _ -> -- traverse over rows
                 fmap (\ line -> concatMap readFlag (zip [1..] line))
                      getLine) -- read each row as a line (string)
                 [1::Int .. 8]
  where readFlag :: (Int,Char) -> [Int]
        readFlag (c,'*') = [c]
        readFlag _ = []

main :: IO ()
main = readInput >>= (print . solve)

