import Util

main = solve 2018 1 Solution { parse = map (read . sanitiseNumber) . lines
                             , part1 = sum
                             , part2 = findDuplicate [] . scanl (+) 0 . cycle }

sanitiseNumber :: String -> String
sanitiseNumber ('+':a) = a
sanitiseNumber a = a

findDuplicate :: Eq a => [a] -> [a] -> a
findDuplicate seen (x:xs)
  | x `elem` seen = x
  | otherwise = findDuplicate (x:seen) xs
