import Util

main = solve 2020 1 Solution { parse = map read . lines
                             , part1 = fmap product . findCountElementsSummingUpTo 2 2020
                             , part2 = fmap product . findCountElementsSummingUpTo 3 2020 }

findCountElementsSummingUpTo :: Int -> Int -> [Int] -> Maybe [Int]
findCountElementsSummingUpTo _ _ [] = Nothing
findCountElementsSummingUpTo 0 _ _ = Nothing
findCountElementsSummingUpTo 1 sum elements
  | sum `elem` elements = Just [sum]
  | otherwise = Nothing
findCountElementsSummingUpTo elementCount sum (x:xs) =
  case matched of
    Nothing -> findCountElementsSummingUpTo elementCount sum xs
    Just matchedElements -> Just (x:matchedElements)
  where matched = findCountElementsSummingUpTo (elementCount - 1) (sum - x) xs
