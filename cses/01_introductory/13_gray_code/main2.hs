-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Gray Code
-- https://cses.fi/problemset/task/2205


-- noticing a pattern,
-- gray code for n bits, beginning with 1, is the gray code for n-1 bits, reversed, prefixed with 1
-- TODO: Explain why this is

gc :: Int -> [String]
gc 1 = ["0", "1"]
gc n = let c = gc (n-1)
       in (map ('0':) c) ++ reverse (map ('1':) c)

main :: IO ()
main = getLine >>= (sequence_ . (map putStrLn) . gc . read )
