-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Tower of Hanoi
-- https://cses.fi/problemset/task/2165

-- The Tower of Hanoi game consists of three stacks (left, middle and right)
-- and n round disks of different sizes. Initially, the left stack has all the disks,
-- in increasing order of size from top to bottom.
-- The goal is to move all the disks to the right stack using the middle stack.
-- On each move you can move the uppermost disk from a stack to another stack.
-- In addition, it is not allowed to place a larger disk on a smaller disk.
-- Your task is to find a solution that minimizes the number of moves.
-- Input
-- The only input line has an integer n: the number of disks.
-- Output
-- First print an integer k: the minimum number of moves.
-- After this, print k lines that describe the moves.
-- Each line has two integers a and b: you move a disk from stack a to stack b.
-- Constraints

-- 1 \le n \le 16

-- Example
-- Input:
-- 2

-- Output:
-- 3
-- 1 2
-- 1 3
-- 2 3

move :: Int -> [(Int,Int)] -> Int -> Int -> Int -> [(Int,Int)]

move 1 acc s _ d = (s,d):acc     -- move a single disk directly from source peg to destination peg

move n acc s t d =
  let a' = move (n-1) acc s d t  -- move n-1 disks from source peg to temp peg using destination peg as temporary
      a'' = (s,d):a'             -- move remaining disk (largest) from source peg to destination peg
  in move (n-1) a'' t s d        -- move n-1 disks from temporary peg to destination peg, using source peg as temporary

formatMove :: (Int,Int) -> String
formatMove (s,d) = unwords [show s, show d]

main :: IO ()
main = getLine >>= (sequence_ . (\n -> (\ms -> (print (length ms)):(map (putStrLn . formatMove) (reverse ms))) (move n [] 1 2 3)) . read)

