-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Gray Code
-- https://cses.fi/problemset/task/2205


-- noticing a pattern,
-- gray code for n bits, beginning with 1, is the gray code for n-1 bits, reversed, prefixed with 1
-- TODO: Explain why this is

-- Note that reversing a Gray code sequence preserves
-- the property that consecutive codes differ by one bit.
-- At each level, the sequence is the joining of the shorter sequence
-- to itself. The first half is prefixed by 0 and the second half is prefixed by 1.
-- Since these prefix bits are constant, the shorter sequence is still valid.
-- The transition from 0-prefix to 1-prefix is also valid because
-- the end of the first, 0-prefixed, shorter sequence is the first element
-- of the second, reversed copy. Therefore, only the prefix bit changes,
-- preserving the invariant rule.

-- Example, joining two 3-bit sequences to form a 4-bit sequence:
-- 0 000  \
-- 0 001  | shorter sequence 000-100
-- 0 011  |
-- 0 010  |
-- 0 110  |
-- 0 111  |
-- 0 101  |
-- 0 100 / -- 0-100
-- 1 100   -- meets 1-100
-- 1 101
-- 1 111
-- 1 110
-- 1 010
-- 1 011
-- 1 001
-- 1 000

gc :: Int -> [String]
gc 1 = ["0", "1"]
gc n = let c = gc (n-1)
       in (map ('0':) c) ++ reverse (map ('1':) c)

main :: IO ()
main = getLine >>= (sequence_ . (map putStrLn) . gc . read)
