-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Two Knights
-- https://cses.fi/problemset/task/1072

-- AKA https://oeis.org/A014635 - No, I did not cheat! I solved it first.
-- Curious link to The Ball Hating Monster https://oeis.org/A104188


board :: Int -> [Int]

board maxk =
  [ let moves::[Int] = [1 | r <- [1..k], c <- [1..k],   -- first knight is placed on every board square
                            a <- [-2..2], b <- [-2..2], -- the 5x5 neighbourhood around it
                            abs(a)+abs(b) /= 3,         -- exclude the 2nd knight threat positions
                            a /= 0 || b /= 0,           -- exclude position of first knight
                            (r+a) >= 1, (r+a) <= k, (c+b) >= 1, (c+b) <= k] -- exclude locations outside board
    in (length moves) `div` 2
    | k <- [1..maxk] ]


main :: IO ()

main = getLine >>= (print . board . read)


