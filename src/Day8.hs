module Day8 where

import Data.Char (digitToInt, intToDigit)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


puzzleData :: IO (Vector (Vector Text))
puzzleData = TIO.readFile "input/input8.txt" >>= return . parseFile numRows numColumns


parseFile :: Int -> Int -> Text -> Vector (Vector Text)
parseFile rows cols = V.fromList . map (V.fromList . T.chunksOf cols) . T.chunksOf (rows * cols)


numRows :: Int
numRows = 6


numColumns :: Int
numColumns = 25


numberContained :: Char -> Text -> Int
numberContained c = T.length . T.filter (== c)


solvePart1 :: Vector (Vector Text) -> Int
solvePart1 layers = answerFor chosenLayer
    where chosenLayer = V.minimumBy (comparing numZeros) layers
          numZeros = sum . fmap (numberContained '0')
          answerFor = (*) <$> totalNumber '1' <*> totalNumber '2'
          totalNumber c = sum . fmap (numberContained c)


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


data Colour = Black | White | Transparent deriving (Enum)

data FinalColour = FinalBlack | FinalWhite deriving (Enum)


translateColour :: Char -> Colour
translateColour = toEnum . digitToInt


stackLayers :: [Colour] -> FinalColour
stackLayers [] = error "no colour found, all layers transparent!"
stackLayers (c:cs) = case c of
    Black -> FinalBlack
    White -> FinalWhite
    Transparent -> stackLayers cs


colourOfPosition :: Int -> Int -> Int -> Vector (Vector Text) -> Colour
colourOfPosition layer row col = translateColour . flip T.index col . (! row) . (! layer)


getStackAtPosition :: Int -> Int -> Vector (Vector Text) -> [Colour]
getStackAtPosition row col v = map (\l -> colourOfPosition l row col v) [0..(V.length v - 1)]


getPicture :: Vector (Vector Text) -> [[FinalColour]]
getPicture layers = map (map (uncurry convertColour)) allPositions
    where allPositions = V.toList . V.imap (\i row -> map (\n -> (i, n)) row) . V.fromList
                            $ replicate numRows [0..(numColumns - 1)]
          convertColour row col = stackLayers $ getStackAtPosition row col layers


convertPicture :: [[FinalColour]] -> [String]
convertPicture = map $ map makeVisible
    where makeVisible FinalBlack = ' '
          makeVisible FinalWhite = 'X'


thePicture :: IO [String]
thePicture = puzzleData >>= return . convertPicture . getPicture
