module Day18 where

import Data.Char (isUpper, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Tree (Tree(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


data Position = Start | Empty | Wall | Key Char | Door Char deriving (Eq)

type Maze = Vector (Vector Position)


translatePosition :: Char -> Position
translatePosition '#' = Wall
translatePosition '.' = Empty
translatePosition '@' = Start
translatePosition c
    | isUpper c = Door c
    | otherwise = Key $ toUpper c


puzzleData :: IO Maze
puzzleData = TIO.readFile "input/input18.txt" >>= return . parseFile


parseFile :: Text -> Maze
parseFile = V.fromList . map parseLine . T.lines
    where parseLine = V.fromList . map translatePosition . T.unpack


type Location = (Int, Int)


lookupLocation :: Maze -> Location -> Position
lookupLocation m (x, y) = (m ! y) ! x


width :: Maze -> Int
width = V.length . (! 0)


height :: Maze -> Int
height = V.length

{-
In order to solve the puzzle, we're going to translate the maze grid into a tree.
This won't work for all conceivable mazes (eg if they have loops), but from inspection
it seems that this particular maze can be so represented.
Each node will be represented by the following data:
- an Int, showing how many steps need to be taken to reach that maze section after entering its parent
- the direction that you need to go in to enter the passage in the first place
- a list of pairs representing, in order, what keys/doors can be found inside it. Each pair contains a
Position (key or door) and an integer giving how many steps along the passage it can be found
- Note that the root of the tree will by convention be represented by the value (0, North, [Start, 0]).
Also the maze starts in a slightly wider chamber compared to the narrow passages we're assuming elsewhere,
it seems that all the passages start at distance exactly three from the central start point, so that's the
assumption I'll make.
-}

data Direction = North | South | East | West deriving (Eq, Enum, Bounded)

type MazeAsTree = Tree (Int, Direction, [(Position, Int)])


startLocation :: Maze -> Location
startLocation m = head [(x, y) | x <- [0..width m], y <- [0..height m], lookupLocation m (x, y) == Start]


walk :: Direction -> Location -> Location
walk North (x, y) = (x, y - 1)
walk South (x, y) = (x, y + 1)
walk East  (x, y) = (x + 1, y)
walk West  (x, y) = (x - 1, y)


-- a simple utility function which will give all the open directions (from a max of 4)
-- that can be gone in from there
directionsToGo :: Maze -> Location -> [Direction]
directionsToGo m l = filter ((/= Wall) . lookupLocation m . flip walk l) [minBound..maxBound]


-- the following is the key function in transforming the maze grid into
-- a tree structure. It takes as input, as well as the maze grid, a location and a direction,
-- follows the passage until it either hits a dead end or splits up, and records that particular
-- subtree (recursively, as far as all dead ends).
followPassage :: Maze -> Location -> Direction -> MazeAsTree
followPassage m l dir = go 1 l dir [] 0
    where go distance l dir found distanceToReach =
            let foundHere = case lookupLocation m l of
                    Empty -> []
                    Start -> []
                    Wall -> error "wtf, I'm standing on a wall??"
                    Key c -> [(Key c, distance)]
                    Door c -> [(Door c, distance)]
                opposite North = South
                opposite South = North
                opposite East  = West
                opposite West  = East
            in case filter (/= opposite dir) (directionsToGo m l) of
                [] -> Node (distanceToReach, dir, foundHere ++ found) [] -- (dead end)
                [d] -> go (distance + 1) (walk d l) d (foundHere ++ found) distanceToReach
                -- (normal case, can only go forward or back, so keep going forward)
                dirs -> Node (distanceToReach, dir, foundHere ++ found)
                            $ map (\d -> go 1 (walk d l) d [] distance) dirs
-- partially working, but definitely some bugs, need to come back to
