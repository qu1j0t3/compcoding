-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Repetitions
-- https://cses.fi/problemset/task/1069

-- This was a nice point-free hint by traxex

import Data.List

main :: IO ()

-- first, apply group to the parameter,
-- then apply (fmap length), then maximum, then print
main = getLine >>= (print . maximum . (fmap length) . group)
