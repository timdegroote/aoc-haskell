import Util

main = solve 2018 2 Solution { parse = lines
                             , part1 = reduceTuple (*) . countTwoThrees . map (\s -> (hasOccurrencyCount 2 s, hasOccurrencyCount 3 s))
                             , part2 = head }

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

combineTuples :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
combineTuples cmbn (a1, b1) (a2, b2) = (cmbn a1 a2, cmbn b1 b2)

reduceTuple :: (a -> b -> c) -> (a, b) -> c
reduceTuple reducer (a, b) = reducer a b

countTwoThrees :: [(Bool, Bool)] -> (Int, Int)
countTwoThrees [] = (0, 0)
countTwoThrees ((hasTwos, hasThrees):xs) =
  combineTuples (+) (fromEnum hasTwos, fromEnum hasThrees) (countTwoThrees xs)
