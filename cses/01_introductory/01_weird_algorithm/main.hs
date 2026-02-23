-- import Numeric (readInt)


main :: IO ()

step :: Int -> IO ()

step 1 = print 1
step x | even x = print x >>= (\_ -> step (x `div` 2))
       | otherwise = print x >>= (\_ -> step ((x * 3) + 1))


main = do
  line <- getLine
  let n::Int = read line
  step n
