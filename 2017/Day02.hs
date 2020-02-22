import Util
import Data.List

main = solve 2017 2 Solution { parse = map (map (\s -> read s :: Int) . words) . lines
                             , part1 = sum . map (\xs -> maximum xs - minimum xs)
                             , part2 = sum . map (\(Just (a, b)) -> div a b) . map findEvenlyDivisible }

findEvenlyDivisible :: [Int] -> Maybe (Int, Int)
findEvenlyDivisible xs =
  go $ reverse $ sort xs
  where
    go [] = Nothing
    go (x:xs) =
      case evenlyDivisible of
        (Just val) -> Just (x, val)
        Nothing -> go xs
      where evenlyDivisible = find (\y -> mod x y == 0) xs
