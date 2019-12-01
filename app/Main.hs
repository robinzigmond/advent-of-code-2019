module Main where

import qualified Day1

main :: IO ()
main = do
    putStr "The answer to Day 1, Part 1 is "
    d1p1 <- Day1.part1
    putStrLn $ show d1p1
    putStr "The answer to Day 1, Part 2 is "
    d1p2 <- Day1.part2
    putStrLn $ show d1p2
