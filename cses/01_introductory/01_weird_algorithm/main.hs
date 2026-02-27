-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

step :: Int -> IO ()

step 1 = print 1
step x | even x    = print x >>= (\_ -> step (x `div` 2))
       | otherwise = print x >>= (\_ -> step ((x * 3) + 1))


main :: IO ()

main = getLine >>= (\line -> step (read line))
