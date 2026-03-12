-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Two Knights
-- https://cses.fi/problemset/task/1072

-- My first result was wrong but led me to the Ball Hating Monster https://oeis.org/A104188

-- The correct sequence is (spoilers): https://oeis.org/A172132

board :: Int -> [Int]

board maxk = map moves [1..maxk]


moves :: Int -> Int

moves 1 = 0
moves 2 = 4*3 `div` 2
moves 3 = (8*(3*3-3)          -- if first knight is in an edge position, it's always threatened by 2 other knight moves
          + (3*3-1)) `div` 2  -- if first knight is in centre, other knight can be in any edge position
-- When the board is at least 4x4, then there are six
-- threat configurations: the central (k-4) x (k-4) squares are threatened by all 8 knight moves,
-- then the adjacent strips of k-4 squares in each direction (N,E,S,W) are threatened by 6 knight moves
-- then the edge strips of k-4 squares are threatened by 4 knight moves
-- then there are four corner squares, in each of the four corners
-- +---+---+- -
-- | A | B |
-- +---+---+- -
-- | C | D |
-- +---+---+- -
-- :   :   :
moves k = ((k-4)*(k-4) * (k*k - 9) -- centre square
           + 4 * (k-4) * (k*k - 7) -- inner strips
           + 4 * (k-4) * (k*k - 5) -- edge strips
           + 4 * (k*k - 5) -- corner square D (4 threats)
           + 8 * (k*k - 4) -- corner squares B, C (3 threats each)
           + 4 * (k*k - 3) -- corner square A (2 threats)
          ) `div` 2

-- Input:
-- 8

-- Output:
-- 0
-- 6
-- 28
-- 96
-- 252
-- 550
-- 1056
-- 1848


main :: IO ()

main = getLine >>= (sequence_ . (map (putStrLn . show)) . board . read)


