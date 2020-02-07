import Util

main = solve 2018 2 Solution { parse = lines
                             , part1 = reduceTuple (*) . countTwoThrees . map (\s -> (hasOccurrencyCount 2 s, hasOccurrencyCount 3 s))
                             , part2 = foldl1 getMatchingElements . findDiffsAtMost 1}

countOccurrencies :: (Eq a) => a -> [a] -> Int
countOccurrencies _ [] = 0
countOccurrencies i (x:xs)
  | i == x = 1 + countOccurrencies i xs
  | otherwise = countOccurrencies i xs

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise = x:(unique xs)

hasOccurrencyCount :: (Eq a) => Int -> [a] -> Bool
hasOccurrencyCount n xs =
  any ((== n) . (\el -> countOccurrencies el xs)) (unique xs)

combineTuples :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
combineTuples cmbn (a1, b1) (a2, b2) = (cmbn a1 a2, cmbn b1 b2)

reduceTuple :: (a -> b -> c) -> (a, b) -> c
reduceTuple reducer (a, b) = reducer a b

countTwoThrees :: [(Bool, Bool)] -> (Int, Int)
countTwoThrees [] = (0, 0)
countTwoThrees ((hasTwos, hasThrees):xs) =
  combineTuples (+) (fromEnum hasTwos, fromEnum hasThrees) (countTwoThrees xs)

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
