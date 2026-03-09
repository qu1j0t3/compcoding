-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Two Knights
-- https://cses.fi/problemset/task/1072

-- My first result was wrong but led me to the Ball Hating Monster https://oeis.org/A104188

-- The correct sequence is (spoilers): https://oeis.org/A172132

board :: Int -> [Int]

board maxk =
  [ let moves::[Int] = [1 | r <- [1..k], c <- [1..k],     -- first knight can be placed on every board square
                            r' <- [1..k], c' <- [1..k],   -- second knight
                            (r',c') /= (r,c),             -- can't be same position
                            abs (r'-r) + abs (c'-c) /= 3 || abs (r'-r) > 2 || abs (c'-c) > 2] -- can't threaten first knight (one knight move away)
    in (length moves) `div` 2
    | k <- [1..maxk] ]
        -- IMPORTANT NOTE FOR FUTURE
        -- I spent far too long here not seeing that 'abs (r'-r) + abs (c'-c) /= 3'
        -- includes moves outside the 5x5 neighbourhood!
        -- e.g. 3 squares directly horizontal or vertical.
        -- I had convinced myself this logic was correct partly because
        -- I only tested on a 5x5 generator e.g. [-2..2]x[-2..2] !

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


