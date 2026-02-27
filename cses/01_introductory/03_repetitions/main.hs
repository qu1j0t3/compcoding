-- Copyright (C) 2026 Toby Thain, toby@telegraphics.net

-- Solution to CSES Introductory Problems Repetitions
-- https://cses.fi/problemset/task/1069


step :: Int -> Int -> Char -> IO Int

step count longest currentChar =
  do next <- getChar
     case next of
       '\n' -> return longest
       _ -> if next == currentChar
            then step (count+1) longest currentChar
            else step 1 (max count longest) next


main :: IO ()

main = do first <- getChar
          longest <- step 1 0 first
          print longest
