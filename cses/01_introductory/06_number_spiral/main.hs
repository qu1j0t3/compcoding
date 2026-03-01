-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Number Spiral
-- https://cses.fi/problemset/task/1071

-- import Text.Printf


test :: [Int] -> Int

test [y,x] = getIndex y x


runTests :: Int -> IO ()

runTests count = sequence_ [ getLine >>= (print . test . (map read) . words) | _ <- [1..count]]


getIndex :: Int -> Int -> Int

getIndex y x = let rank = max y x
                   (y',x') = if odd rank then (y,x) else (x,y)
               in rank*rank - (if y' > x' then 2*rank-1-x' else y'-1)


-- For testing
printSpiral :: Int -> IO ()
printSpiral size = sequence_ [ putStrLn (show [getIndex r c | c <- [1..size]]) | r <- [1..size] ]


main :: IO ()

main = getLine >>= (runTests . read)


