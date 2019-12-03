{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)


type Point = (Int, Int)


manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y


puzzleData :: IO (Set Point, Set Point)
puzzleData = TIO.readFile "input/input3.txt" >>= return . parseFile


parseLine :: Text -> Set Point
parseLine = followLine . T.splitOn ","
    where followLine = go S.empty (0, 0)
          go s _ [] = s
          go s (x, y) (dir:dirs) =
            let dist = (read . T.unpack . T.tail) dir
                endpoint = case (T.head dir) of
                    'U' -> (x, y + dist)
                    'D' -> (x, y - dist)
                    'R' -> (x + dist, y)
                    'L' -> (x - dist, y)
                insertion = case (T.head dir) of
                    'U' -> foldr S.insert s $ map (\n -> (x, n)) [(y+1)..(y+dist)]
                    'D' -> foldr S.insert s $ map (\n -> (x, n)) [(y-1),(y-2)..(y-dist)]
                    'R' -> foldr S.insert s $ map (\n -> (n, y)) [(x+1)..(x+dist)]
                    'L' -> foldr S.insert s $ map (\n -> (n, y)) [(x-1),(x-2)..(x-dist)]
            in go insertion endpoint dirs


parseFile :: Text -> (Set Point, Set Point)
parseFile t = let [l1, l2] = T.lines t in (parseLine l1, parseLine l2)


solvePart1 :: (Set Point, Set Point) -> Int
solvePart1 (s1, s2) = minimum . map manhattan . S.toList $ S.intersection s1 s2


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


{- not the most efficient, as this goes over the whole paths again in order to trace the distance,
having already found the intersections. But it should be quick enough, and easier than rewriting
the earlier code to annonate each point with a distance, or store the points as lists instead of sets.

Although this does violate DRY by repeating the basic logic of the parseLine function above :/ -}

-- strictly speaking should return a Maybe Int, but don't need to bother here as each point we test on
-- is guaranteed to be reachable along the line
distanceAlongLine :: Point -> Text -> Int
distanceAlongLine (x0, y0) = getDistance (0, 0) . T.splitOn ","
    where getDistance = go 0
          go n _ [] = error "point not found along line"
          go n (x, y) (dir:dirs) =
            let dist = (read . T.unpack . T.tail) dir
                endpoint = case (T.head dir) of
                    'U' -> (x, y + dist)
                    'D' -> (x, y - dist)
                    'R' -> (x + dist, y)
                    'L' -> (x - dist, y)
                ifNotFound = go (n + dist) endpoint dirs
            in case (T.head dir) of
                'U' -> if (x == x0) && (y0 `elem` [(y+1)..(y+dist)]) then (n + y0 - y) else ifNotFound
                'D' -> if (x == x0) && (y0 `elem` [(y-1),(y-2)..(y-dist)]) then (n + y - y0) else ifNotFound
                'R' -> if (y == y0) && (x0 `elem` [(x+1)..(x+dist)]) then (n + x0 - x) else ifNotFound
                'L' -> if (y == y0) && (x0 `elem` [(x-1),(x-2)..(x-dist)]) then (n + x - x0) else ifNotFound


solvePart2 :: Text -> Int
solvePart2 t = let [l1, l2] = T.lines t
                   s1 = parseLine l1
                   s2 = parseLine l2
                   totalDistance p = distanceAlongLine p l1 + distanceAlongLine p l2
               in minimum . map totalDistance . S.toList $ S.intersection s1 s2


part2 :: IO Int
part2 = TIO.readFile "input/input3.txt" >>= return . solvePart2
