module EnergyPoints (calculateEnergyPoints) where

import Data.List (nub)

-- Calculates all multiples of a number less than the limit
multiplesBelow :: Int -> Int -> [Int]
multiplesBelow base limit = [x | x <- [1..(limit - 1)], x `mod` base == 0]

-- Main function: calculates energy points for a given level and magical item base values
calculateEnergyPoints :: Int -> [Int] -> Int
calculateEnergyPoints level bases =
  let allMultiples = concatMap (`multiplesBelow` level) bases
      uniqueMultiples = nub allMultiples
  in sum uniqueMultiples
