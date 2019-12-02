module Main where

import qualified Day1
import qualified Day2


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
