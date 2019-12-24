module Day24 where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as V


type Location = (Int, Int)


data Position = Bug | Empty deriving (Eq, Show)


type Area = Vector (Vector Position)


lookupLocation :: Area -> Location -> Maybe Position
lookupLocation a (x, y) = (a !? y) >>= (!? x)


translatePosition :: Char -> Position
translatePosition '#' = Bug
translatePosition '.' = Empty


puzzleData :: IO Area
puzzleData = TIO.readFile "input/input24.txt" >>= return . parseFile


parseFile :: Text -> Area
parseFile = V.fromList . map parseLine . T.lines
    where parseLine = V.fromList . map translatePosition . T.unpack


neighbours :: Location -> [Location]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


numLiveNeighbours :: Area -> Location -> Int
numLiveNeighbours a = length . filter ((== Just Bug) . lookupLocation a) . neighbours


newPosition :: Area -> Location -> Position
newPosition a l = case lookupLocation a l of
    Just Bug
        | numLiveNeighbours a l == 1 -> Bug
        | otherwise -> Empty
    Just Empty
        | numLiveNeighbours a l `elem` [1, 2] -> Bug
        | otherwise -> Empty
    Nothing -> error "tried to look up a position that's out of bounds!"


generation :: Area -> Area
generation a = V.imap updateRow a
    where updateRow rowNo = V.imap (update rowNo)
          update rowNo colNo _ = newPosition a (colNo, rowNo)


convertToNumber :: Area -> Int
convertToNumber = toBinary . toBooleanList
    where toBooleanList = concat . V.toList . fmap (map toBool . V.toList)
          toBool = (== Bug)
          toBinary [] = 0
          toBinary (True:bs) = 1 + 2 * toBinary bs
          toBinary (False:bs) = 2 * toBinary bs


solvePart1 :: Area -> Int
solvePart1 a = go a []
    where go current previous
            = let next = generation current
                  asNumber = convertToNumber next
              in if asNumber `elem` previous
                    then asNumber
                    else go next (asNumber:previous)


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


-- Part 2 is quite scary when first read, but actually shouldn't be too bad. As often, it's all about
-- using an appropriate data structure. And we need to rewrite virtually all of the above functions
-- to work in the new setting.

-- Note that we work with an IntMap of Areas representing 5x5 grids, despite the fact that the middle
-- (2, 2) cell of each grid is not used. Because of this, there are a couple of points where we need
-- to explicitly take this into account: when counting the neighbours to apply the automaton rules,
-- and at the end when counting bugs.


type RecursiveGrid = IntMap Area

type RecursiveLocation = (Int, Location)


makeRecursive :: Area -> RecursiveGrid
makeRecursive = IM.singleton 0


lookupLocationRecursive :: RecursiveGrid -> RecursiveLocation -> Position
lookupLocationRecursive g (n, l)
    = fromMaybe Empty $ lookupLocation (IM.findWithDefault V.empty n g) l


neighboursRecursive :: RecursiveLocation -> [RecursiveLocation]
neighboursRecursive (n, l)
    = concat [map (\l' -> (n, l')) (filter (/= (2, 2)) $ neighbours l),
                map (\l' -> (n - 1, l')) (upperNeighbours l),
                map (\l' -> (n + 1, l')) (lowerNeighbours l)]
    where upperNeighbours (0, 0) = [(2, 1), (1, 2)]
          upperNeighbours (0, 4) = [(1, 2), (2, 3)]
          upperNeighbours (4, 0)  = [(2, 1), (3, 2)]
          upperNeighbours (4, 4) = [(3, 2), (2, 3)]
          upperNeighbours (0, _) = [(1, 2)]
          upperNeighbours (4, _) = [(3, 2)]
          upperNeighbours (_, 0) = [(2, 1)]
          upperNeighbours (_, 4) = [(2, 3)]
          upperNeighbours _ = []
          lowerNeighbours (1, 2) = map (\n -> (0, n)) [0..4]
          lowerNeighbours (2, 1) = map (\n -> (n, 0)) [0..4]
          lowerNeighbours (3, 2) = map (\n -> (4, n)) [0..4]
          lowerNeighbours (2, 3) = map (\n -> (n, 4)) [0..4]
          lowerNeighbours _ = []


numLiveNeighboursRecursive :: RecursiveGrid -> RecursiveLocation -> Int
numLiveNeighboursRecursive g = length . filter ((== Bug) . lookupLocationRecursive g) . neighboursRecursive


newPositionRecursive :: RecursiveGrid -> RecursiveLocation -> Position
newPositionRecursive g l = case lookupLocationRecursive g l of
    Bug
        | numLiveNeighboursRecursive g l == 1 -> Bug
        | otherwise -> Empty
    Empty
        | numLiveNeighboursRecursive g l `elem` [1, 2] -> Bug
        | otherwise -> Empty


addEmptyLayers :: RecursiveGrid -> RecursiveGrid
addEmptyLayers g = let currentLayers = IM.keys g
                       minLayer = minimum currentLayers
                       maxLayer = maximum currentLayers
                       emptyGrid = V.replicate 5 $ V.replicate 5 Empty
                   in IM.insert (minLayer - 1) emptyGrid $ IM.insert (maxLayer + 1) emptyGrid g


generationRecursive :: RecursiveGrid -> RecursiveGrid
generationRecursive g = IM.mapWithKey updateArea $ addEmptyLayers g
    where updateArea :: Int -> Area -> Area
          updateArea n a = V.imap (updateRow n) a
          updateRow n rowNo = V.imap (update n rowNo)
          update n rowNo colNo _ = newPositionRecursive g (n, (colNo, rowNo))


countBugs :: Area -> Int
countBugs g = V.length (V.concatMap (V.filter (== Bug)) g) - adjust
    where adjust = if ((g ! 2) ! 2) == Bug then 1 else 0


solvePart2 :: Area -> Int
solvePart2 = sum . map countBugs . IM.elems . repeat 200 generationRecursive . makeRecursive
    where repeat 0 _ a = a
          repeat n f a = f $! repeat (n - 1) f a


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
