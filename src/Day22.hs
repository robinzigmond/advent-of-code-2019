module Day22 where

import Control.Monad.State
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V


data Shuffle = NewStack | Cut Int | Increment Int


puzzleData :: IO [Shuffle]
puzzleData = TIO.readFile "input/input22.txt" >>= return . parseFile


parseFile :: Text -> [Shuffle]
parseFile = map (parseLine . T.unpack) . T.lines
    where parseLine "deal into new stack" = NewStack
          parseLine ('d':'e':'a':'l':' ':'w':'i':'t':'h':' ':'i':'n':'c':'r':'e':'m':'e':'n':'t':' ':s)
            = Increment $ read s
          parseLine ('c':'u':'t':' ':s) = Cut $ read s
          parseLine _ = error "unexpected input line"


type Deck = Vector Int

type DeckShuffle = Deck -> Deck



newStack :: DeckShuffle
newStack = V.reverse


cut :: Int -> DeckShuffle
cut n d
    | n >= 0 = let (top, bottom) = V.splitAt n d in V.concat [bottom, top]
    | n < 0 = let (top, bottom) = V.splitAt (V.length d + n) d in V.concat [bottom, top]


increment :: Int -> DeckShuffle
increment n d = let (d', _, _) = execState doIncrement (d, 0, 0) in d'
    where doIncrement :: State (Deck, Int, Int) ()
          doIncrement = do
            (deck, pos, originalPos) <- get
            let newDeck = deck // [(pos, d ! originalPos)]
            let nextPos = (pos + n) `mod` V.length d
            put (newDeck, nextPos, originalPos + 1)
            if nextPos == 0
                then return ()
                else doIncrement


doShuffle :: Shuffle -> DeckShuffle
doShuffle NewStack = newStack
doShuffle (Cut n) = cut n
doShuffle (Increment n) = increment n


allShuffles :: [Shuffle] -> DeckShuffle
allShuffles = foldl' (flip (.)) id . map doShuffle


solvePart1 :: [Shuffle] -> Int
solvePart1 ss = let Just pos = V.elemIndex 2019 (allShuffles ss $ V.fromList [0..10006]) in pos


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1
