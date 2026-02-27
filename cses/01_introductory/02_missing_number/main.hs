-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

missingNumber :: Int -> [Int] -> Int

missingNumber n lst =
  let expectedSum = n*(n+1) `div` 2
      actualSum = foldl (+) 0 lst
  in expectedSum - actualSum


main :: IO ()

main = do line <- getLine
          lst <- getLine
          print (missingNumber (read line) (fmap read (words lst)))

