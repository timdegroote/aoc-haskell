import Util
import Data.Char

main = solve 2017 1 Solution { parse = filter isDigit
                             , part1 = sum . map digitToInt . getElementsMatchingOffset 1
                             , part2 = sum . map digitToInt . (\xs -> getElementsMatchingOffset (div (length xs) 2) xs) }

getElementsMatchingOffset :: Eq a => Int -> [a] -> [a]
getElementsMatchingOffset _ [] = []
getElementsMatchingOffset 0 xs = xs
getElementsMatchingOffset offset xs =
  map fst $ filter (\(a, b) -> a == b) $ take l $ z
  where z = zip (cycle xs) $ drop offset $ cycle xs
        l = length xs

