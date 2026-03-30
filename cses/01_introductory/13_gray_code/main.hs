-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Gray Code
-- https://cses.fi/problemset/task/2205

-- A Gray code is a list of all 2^n bit strings of length n,
-- where any two successive strings differ in exactly one bit (i.e., their Hamming distance is one).
-- Your task is to create a Gray code for a given length n.
-- Input
-- The only input line has an integer n.
-- Output
-- Print 2^n lines that describe the Gray code. You can print any valid solution.
-- Constraints

-- 1 \le n \le 16

-- Example
-- Input:
-- 2

-- Output:
-- 00
-- 01
-- 11
-- 10

-- we can keep a set of all codes seen so far
-- as bits in an Int
-- keep track of current value

-- given a value
-- search for the next value by trying to flip a single bit,
-- try in order from 0 (least significant) towards more significant
  -- flip a bit -> if not in state -> add to state and accum -> continue from new state and bit 0
  --            -> if in state, advance bit index and retry

import Data.Bits

import Numeric
import Data.Char

step :: Int -> Int -> Integer -> Int -> [Int] -> [Int]
step bits i state v acc =
  if i == bits then acc -- all 2^bits states have been constructed; we're finished
  else let newV = v `complementBit` i -- construct possible next value by flipping bit i
       in if (state `testBit` newV) -- if new value has already been seen,
          then step bits (1+ i) state v acc -- continue, trying next more significant bit
          else step bits 0 (state `setBit` newV) newV (newV:acc) -- otherwise add to state and to result accumulator

grayCode :: Int -> [Int]
grayCode bits = reverse (step bits 0 (bit 0) 0 [0])

binaryString :: Int -> Int -> String
binaryString bits x = reverse (take bits (((reverse ((showBin x) "")) ++ (repeat '0'))))

main :: IO ()
main = getLine >>= (sequence_ . (\bits -> (map (putStrLn . (binaryString bits)) (grayCode bits))) . read )
