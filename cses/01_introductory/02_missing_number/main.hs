-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

missingNumber :: Int -> [Int] -> Int

missingNumber n lst = n*(n+1) `div` 2 - sum lst


main :: IO ()

main = do line <- getLine
          lst <- getLine
          print (missingNumber (read line) (fmap read (words lst)))

