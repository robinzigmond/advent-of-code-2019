module Day1 where

import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Data.Text.IO as TIO (readFile)


puzzleData :: IO [Int]
puzzleData = TIO.readFile "input/input1.txt" >>= return . parseFile


parseFile :: Text -> [Int]
parseFile = map (read . T.unpack) . T.lines


computeFuel :: Int -> Int
computeFuel = subtract 2 . flip div 3


solvePart1 :: [Int] -> Int
solvePart1 = sum . map computeFuel


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


computeTotalFuel :: Int -> Int
computeTotalFuel n = go 0 $! computeFuel n
    where
        go cum n
            | n <= 0    = cum
            | otherwise = go (cum + n) $! computeFuel n


solvePart2 :: [Int] -> Int
solvePart2 = sum . map computeTotalFuel

        
part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
