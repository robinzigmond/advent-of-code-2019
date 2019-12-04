module Day4 where

import Control.Applicative (liftA2)
import Data.List (group)


minPassword :: Int
minPassword = 284639


maxPassword :: Int
maxPassword = 748759


digits :: Int -> [Int]
digits = map (read . pure) . show


isNonDecreasing :: (Ord a) => [a] -> Bool
isNonDecreasing [] = True
isNonDecreasing [a] = True
isNonDecreasing (a:b:as) = (a <= b) && isNonDecreasing (b:as)


hasRepeat :: (Eq a) => [a] -> Bool
hasRepeat [] = False
hasRepeat [a] = False
hasRepeat (a:b:as) = (a == b) || hasRepeat (b:as)


solvePart1 :: Int -> Int -> Int
solvePart1 min max = length $ filter (liftA2 (&&) isNonDecreasing hasRepeat . digits) [min..max]


part1 :: Int
part1 = solvePart1 minPassword maxPassword


hasExactPair :: (Eq a) => [a] -> Bool
hasExactPair = (2 `elem`) . (map length . group)


solvePart2 :: Int -> Int -> Int
solvePart2 min max = length $ filter (liftA2 (&&) isNonDecreasing hasExactPair . digits) [min..max]


part2 :: Int
part2 = solvePart2 minPassword maxPassword
