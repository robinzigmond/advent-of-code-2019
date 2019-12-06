{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.IO as TIO (readFile)


puzzleData :: IO (Map Text Text)
puzzleData = TIO.readFile "input/input6.txt" >>= return . parseFile


parseFile :: Text -> Map Text Text
parseFile = foldr insertion M.empty . T.lines
    where insertion line map =
            let [orbited, orbiting] = T.splitOn ")" line in M.insert orbiting orbited map


allOrbits :: Map Text Text -> Text ->  [Text]
allOrbits m orbiting = case M.lookup orbiting m of
    Nothing -> []
    Just orbited -> orbited : allOrbits m orbited


solvePart1 :: Map Text Text -> Int
solvePart1 m = sum . map (length . allOrbits m) $ M.keys m


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


minTransfers :: Map Text Text -> Text -> Text -> Int
minTransfers m src tgt = go 1
    where srcOrbits n = reverse . take n $ src : allOrbits m src
          tgtOrbits n = take n $ tgt : allOrbits m tgt
          maybeIndex n = head (srcOrbits n) `elemIndex` (tgtOrbits n)
          go n = case maybeIndex n of
                    Just i -> n + i - 1
                    Nothing -> go $ n + 1


solvePart2 :: Map Text Text -> Int
solvePart2 m = let whatIAmOrbiting = M.lookup "YOU" m
                   whatSantaIsOrbiting = M.lookup "SAN" m
               in case whatIAmOrbiting of
                    Nothing -> error "I'm not orbiting anything!"
                    Just src -> case whatSantaIsOrbiting of
                                    Nothing -> error "Santa isn't orbiting anything!"
                                    Just tgt -> minTransfers m src tgt

part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
