-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Two Sets
-- https://cses.fi/problemset/task/1092


-- adding numbers from largest to smallest to two sets,
-- each time adding to the smaller sum,
-- produces a sequence that ends in different ways
-- depending on n mod 4.

-- e.g.
-- n mod 4
-- 0       -- YES: {8, 5,4, 1} == {7,6, 3,2}
-- 1       -- NO:  {5, 2,1}    <> {4,3}
-- 2       -- NO:  {6, 3,2}    <> {5,4, 1}
-- 3       -- YES: {7, 4,3}    == {6,5, 2,1}

printList :: [Int] -> IO ()

printList xs = putStrLn (unwords (map show xs))


sets :: Int -> IO ()

sets n = let half = n `div` 2
      in case n `mod` 4 of
        0 -> putStrLn "YES" >> print half >> printList ([1,5..n] ++ [4,8..n])
                            >> print half >> printList ([2,6..n] ++ [3,7..n])
        1 -> putStrLn "NO"
        2 -> putStrLn "NO"
        3 -> putStrLn "YES" >> print (half+1) >> printList ([1,5..n] ++ [2,6..n])
                            >> print half     >> printList ([3,7..n] ++ [4,8..n])

-- main = getLine >>= (sequence_ . (map (putStrLn . show)) . board . read)

main :: IO ()

main = getLine >>= ( sets . read )


