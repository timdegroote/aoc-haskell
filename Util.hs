module Util where

import System.IO

zeroPad :: String -> String
zeroPad (s:[]) = '0':s:[]
zeroPad s = s

data Solution a b c =
  Solution
  { parse :: String -> a
  , part1 :: a -> b
  , part2 :: a -> c}

solve :: (Show b, Show c) => Int -> Int -> Solution a b c -> IO ()
solve year day solution = do
  fileContents <- readFile $ (show year) ++ "/input/day" ++ (zeroPad $ show day) ++ ".txt"
  let parsedInput = parse solution $ fileContents
  putStrLn $ "Part 1: " ++ show (part1 solution $ parsedInput)
  putStrLn $ "Part 2: " ++ show (part2 solution $ parsedInput)
