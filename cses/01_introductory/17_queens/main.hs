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
solve [] = 1            -- base case: placed all rows, so count one solution
solve (b:bs) =          -- place Q in each possible column of row r and return sum of resulting subproblems (r-1)
  sum [ solve ( map (\ (d,cs) -> c:(c+d):(c-d):cs) -- update future rows with new threatened columns based on chosen c
                    (zip [1..] bs) )
        | c <- [1..8], not (elem c b) ]


readInput :: IO [[Int]]
readInput = traverse (\ _ -> -- traverse over rows
                        fmap (\ line -> concatMap readFlag (zip [1..] line))
                              getLine) -- read each row as a line (string)
                        [1::Int .. 8]
  where readFlag :: (Int,Char) -> [Int]
        readFlag (c,'*') = [c]
        readFlag _ = []


main :: IO ()
main = readInput >>= (print . solve)

