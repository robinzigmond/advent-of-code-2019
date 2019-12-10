module Day10 where

import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


{- note that many of the functions below are unsafe, in the sense that unexpected input
will lead to a runtime error. This is a deliberate choice here, as if there is any bad data
(or bad logic leading to unexpected outcomes) then it's better to know about it; the puzzle
data is known to be well-formed! -}


data Position = Asteroid | Empty deriving (Eq)

type Map = Vector (Vector Position)


translatePosition :: Char -> Position
translatePosition '#' = Asteroid
translatePosition '.' = Empty


puzzleData :: IO Map
puzzleData = TIO.readFile "input/input10.txt" >>= return . parseFile


parseFile :: Text -> Map
parseFile = V.fromList . map parseLine . T.lines
    where parseLine = V.fromList . map translatePosition . T.unpack


type Location = (Int, Int)


lookupLocation :: Map -> Location -> Position
lookupLocation m (x, y) = (m ! y) ! x


intermediates :: Location -> Location -> [Location]
intermediates (x1, y1) (x2, y2) = [(x1 + d*dx, y1 + d*dy) | d <- [1..(g-1)]]
    where xdiff = x2 - x1
          ydiff = y2 - y1
          g  = gcd xdiff ydiff
          dx = xdiff `div` g
          dy = ydiff `div` g


canSee :: Map -> Location -> Location -> Bool
canSee m l1 l2
    | l1 == l2 = False -- more convenient later to not count as being able to see your own location
    | otherwise = all (== Empty) . map (lookupLocation m) $ intermediates l1 l2


width :: Map -> Int
width = V.length . (! 0)


height :: Map -> Int
height = V.length


allLocations :: Map -> [Location]
allLocations m = [(x, y) | x <- [0..(width m - 1)], y <- [0..(length m - 1)]]


isAsteroid :: Map -> Location -> Bool
isAsteroid m = (== Asteroid) . lookupLocation m


numberSeen :: Map -> Location -> Int
numberSeen m l = length . filter ((&&) <$> canSee m l <*> isAsteroid m) $ allLocations m


solvePart1 :: Map -> Int
solvePart1 m = maximum . map (numberSeen m) . filter (isAsteroid m) $ allLocations m


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


laserLocation :: Map -> Location
-- finds the actual location of the monitoring station, which we skipped over in part 1
laserLocation m = maximumBy (comparing $ numberSeen m) . filter (isAsteroid m) $ allLocations m


-- in order to find the order in which the asteroids will be destroyed by the laser,
-- we divide space up into 4 quadrants, with the laser at the centre

data Quadrant = NE | SE | SW | NW deriving (Eq, Enum, Bounded)


nextQuadrant :: Quadrant -> Quadrant
nextQuadrant q
    | q == maxBound = minBound
    | otherwise = succ q


getQuadrant :: Location -> Location -> Quadrant
getQuadrant centre l
    | dx >= 0 && dy < 0 = NE
    | dy >= 0 && dx > 0 = SE
    | dx <= 0 && dy > 0 = SW
    | dy <= 0 && dx < 0 = NW
    where (xc, yc) = centre
          (xl, yl) = l
          dx = xl - xc
          dy = yl - yc


-- given a central location ("laser"), a quadrant, and a list of targets, this function extracts
-- those targets in the given quadrant relative to the laser, and sorts them into the order
-- they will be destroyed in
destructionOrder :: Location -> Quadrant -> [Location] -> [Location]
destructionOrder centre q ls = sortOn (quadrantOrdering q) filtered
    where (xc, yc) = centre
          dx (x, _) = x - xc
          dy (_, y) = y - yc
          filtered = filter ((== q) . getQuadrant centre) ls
          quadrantOrdering NE l = abs $ fromIntegral (dx l) / fromIntegral (dy l)
          quadrantOrdering SE l = abs $ fromIntegral (dy l) / fromIntegral (dx l)
          quadrantOrdering SW l = abs $ fromIntegral (dx l) / fromIntegral (dy l)
          quadrantOrdering NW l = abs $ fromIntegral (dy l) / fromIntegral (dx l)


-- note, this isn't as general as it could be - I wanted to account for going round the full
-- 360 degrees more than once if needed, but ran into problems that weren't easily solved.
-- Since both the example and the puzzle feature more than 200 asteroids then a simple
-- single rotation is enough, which importantly means the map doesn't have to be updated
solvePart2 :: Map -> Int
solvePart2 m = getSolution $ go 200 NE m
    where getSolution (x, y) = 100 * x + y
          laser = laserLocation m
          visible = filter ((&&) <$> canSee m laser <*> isAsteroid m) $ allLocations m
          quadrantTargets q = destructionOrder laser q visible
          go n q m
            | n <= length (quadrantTargets q) = quadrantTargets q !! (n - 1)
            | otherwise = go (n - length (quadrantTargets q)) (nextQuadrant q) m


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
