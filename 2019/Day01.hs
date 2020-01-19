import Util

main = solve 2019 1 Solution { parse = map read . lines
                             , part1 = sum . map getRequiredFuelForMass
                             , part2 = sum . map getTotalFuel }

getRequiredFuelForMass :: Int -> Int
getRequiredFuelForMass = subtract 2 . (`div` 3)

getTotalFuel :: Int -> Int
getTotalFuel mass
  | fuel <= 0 = 0
  | otherwise = fuel + getTotalFuel fuel
  where fuel = getRequiredFuelForMass mass
