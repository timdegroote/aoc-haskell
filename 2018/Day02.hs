import Util
import Data.List (nub)
import qualified Data.Map as M

main = solve 2018 2 Solution { parse = lines
                             , part1 = M.foldl (*) 1 . M.fromListWith (+) . concatMap (getOccurrencyCounts [2, 3])
                             , part2 = foldl1 getMatchingElements . findDiffsAtMost 1}

countOccurrencies :: (Eq a) => a -> [a] -> Int
countOccurrencies _ [] = 0
countOccurrencies i (x:xs)
  | i == x = 1 + countOccurrencies i xs
  | otherwise = countOccurrencies i xs

hasOccurrencyCount :: (Eq a) => Int -> [a] -> Bool
hasOccurrencyCount n xs =
  any ((== n) . (\el -> countOccurrencies el xs)) (nub xs)

getOccurrencyCounts :: Eq a => [Int] -> [a] -> [(Int, Int)]
getOccurrencyCounts counts items =
  map (\count -> (count, fromEnum $ hasOccurrencyCount count items)) counts

diffsAtMost :: (Eq a) => Int -> [a] -> [a] -> Bool
diffsAtMost _ [] [] = True
diffsAtMost maxDiff (x:xs) (y:ys)
  | x == y = diffsAtMost maxDiff xs ys
  | maxDiff == 0 = False
  | otherwise = diffsAtMost (pred maxDiff) xs ys

findDiffsAtMost :: (Eq a) => Int -> [[a]] -> [[a]]
findDiffsAtMost _ [] = []
findDiffsAtMost _ (x:[]) = [x]
findDiffsAtMost maxDiff (x:xs)
  | length found >= 1 = x : found
  | otherwise = findDiffsAtMost maxDiff xs
  where found = filter (diffsAtMost maxDiff x) xs

getMatchingElements :: (Eq a) => [a] -> [a] -> [a]
getMatchingElements _ [] = []
getMatchingElements [] _ = []
getMatchingElements (x:xs) (y:ys)
  | x == y = x : getMatchingElements xs ys
  | otherwise = getMatchingElements xs ys
