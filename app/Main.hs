module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5


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
