module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day21
import qualified Day22


main :: IO ()
main = do
    putStr "The answer to Day 1, Part 1 is "
    Day1.part1 >>= print
    putStr "The answer to Day 1, Part 2 is "
    Day1.part2 >>= print
    putStr "The answer to Day 2, Part 1 is "
    Day2.part1 >>= print
    putStr "The answer to Day 2, Part 2 is "
    Day2.part2 >>= print
    putStr "The answer to Day 3, Part 1 is "
    Day3.part1 >>= print
    putStr "The answer to Day 3, Part 2 is "
    Day3.part2 >>= print
    putStr "The answer to Day 4, Part 1 is "
    print Day4.part1
    putStr "The answer to Day 4, Part 2 is "
    print Day4.part2
    putStr "The answer to Day 5, Part 1 is "
    Day5.part1 >>= print
    putStr "The answer to Day 5, Part 2 is "
    Day5.part2 >>= print
    putStr "The answer to Day 6, Part 1 is "
    Day6.part1 >>= print
    putStr "The answer to Day 6, Part 2 is "
    Day6.part2 >>= print
    putStr "The answer to Day 7, Part 1 is "
    Day7.part1 >>= print
    putStr "The answer to Day 7, Part 2 is "
    Day7.part2 >>= print
    putStr "The answer to Day 8, Part 1 is "
    Day8.part1 >>= print
    putStrLn "Here is the picture for Day 8, Part 2:"
    pictureRows <- Day8.thePicture
    mapM_ putStrLn pictureRows
    putStr "The answer to Day 9, Part 1 is "
    Day9.part1 >>= print
    putStr "The answer to Day 9, Part 2 is "
    Day9.part2 >>= print
    putStr "The answer to Day 10, Part 1 is "
    Day10.part1 >>= print
    putStr "The answer to Day 10, Part 2 is "
    Day10.part2 >>= print
    putStr "The answer to Day 11, Part 1 is "
    Day11.part1 >>= print
    putStrLn "Here is the picture for Day 11, Part 2:"
    pictureRows <- Day11.drawPart2
    mapM_ putStrLn pictureRows
    putStr "The answer to Day 12, Part 1 is "
    Day12.part1 >>= print
    putStr "The answer to Day 12, Part 2 is "
    Day12.part2 >>= print
    putStr "The answer to Day 13, Part 1 is "
    Day13.part1 >>= print
    putStr "The answer to Day 13, Part 2 is "
    Day13.part2 >>= print
    putStr "The answer to Day 14, Part 1 is "
    Day14.part1 >>= print
    {-putStr "The answer to Day 14, Part 2 is "
    Day14.part2 >>= print-}
    putStr "The answer to Day 15, Part 1 is "
    print Day15.part1
    putStr "The answer to Day 16, Part 1 is "
    Day16.part1 >>= putStrLn
    {-putStr "The answer to Day 16, Part 2 is "
    Day16.part2 >>= putStrLn-}
    putStr "The answer to Day 17, Part 1 is "
    Day17.part1 >>= print
    putStr "The answer to Day 17, Part 2 is "
    Day17.part2 >>= print
    {-putStr "The answer to Day 18, Part 1 is "
    Day18.part1 >>= print-}
    putStr "The answer to Day 19, Part 1 is "
    Day19.part1 >>= print
    {-putStr "The answer to Day 19, Part 2 is "
    Day19.part2 >>= print-}
    putStr "The answer to Day 21, Part 1 is "
    Day21.part1 >>= print
    putStr "The answer to Day 21, Part 2 is "
    Day21.part2 >>= print
    putStr "The answer to Day 22, Part 1 is "
    Day22.part1 >>= print
    putStr "The answer to Day 22, Part 2 is "
    Day22.part2 >>= print
