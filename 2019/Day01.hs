import Util

main = solve 2019 1 Solution { part1 = solvePart1
                    , part2 = solvePart2
                    , parse = map read . lines }

getRequiredFuelForMass :: Int -> Int
getRequiredFuelForMass = subtract 2 . (`div` 3)

getTotalFuel :: Int -> Int
getTotalFuel mass
  | fuel <= 0 = 0
  | otherwise = fuel + getTotalFuel fuel
  where fuel = getRequiredFuelForMass mass

solvePart1 :: [Int] -> Int
solvePart1 = sum . map getRequiredFuelForMass

solvePart2 :: [Int] -> Int
solvePart2 = sum . map getTotalFuel
