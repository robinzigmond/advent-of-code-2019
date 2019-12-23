module Day22 where

import Control.Monad.State
import Crypto.Number.ModArithmetic (inverseCoprimes)
import Data.List (foldl', elemIndex)
import Data.Semigroup (stimes)
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


-- note: the following shuffle functions are no longer used in the solution, since the new way I did Part 2
-- is much more efficient - but I left them here for interest

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


solvePart1Old :: [Shuffle] -> Int
solvePart1Old ss = let Just pos = V.elemIndex 2019 (allShuffles ss $ V.fromList [0..10006]) in pos


-- new solution, which is vastly more efficient: see the later code for definitions
solvePart1 :: [Shuffle] -> Int
solvePart1 ss = fromInteger $ applyChange 10007 (totalPosChange ss) 2019


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1

{-
Part 2: clearly impossible to do it naively. But there is a change from part 1, other than the huge
numbers: instead of being asked for the position of a fixed card, we're asked for the card at a fixed
position. That means we need only track which card ends in that position - and therefore how the positions
of a fixed card change on each shuffle.

Say the deck has size N. Then the card at position k ends up at the following position after each shuffle type:

- NewStack: N - 1 - k = - 1 - k (mod N)
- cut n: k - n (mod N)
- increment n: n*k (mod N)

This is relatively straightforward - but unfortunately there are 100 different shuffle types done in sequence,
too many to sensibly look at manually. But luckily Haskell lends itself quite well to finding the general
answer!
-}

data PosChange = PosChange {multiplyBy :: !Integer, thenAdd :: !Integer} deriving (Show, Eq)


reduceChange :: Integer -> PosChange -> PosChange
reduceChange b (PosChange m a) = PosChange (m `mod` b) (a `mod` b)


applyChange :: Integer -> PosChange -> Integer -> Integer
applyChange base (PosChange m a) k = (m * k + a) `mod` base


findChange :: Shuffle -> PosChange
findChange NewStack = PosChange (-1) (-1) -- \k -> -1 - k
findChange (Cut n) = PosChange 1 (toInteger (-n)) -- \k -> k - n
findChange (Increment n) = PosChange (toInteger n) 0 -- \k -> n * k


combinePosChange :: PosChange -> PosChange -> PosChange
-- a2 + m2 * (a1 + m1*k) = m1*m2*k + m2*a1 + a2
combinePosChange (PosChange m1 a1) (PosChange m2 a2) = PosChange (m1 * m2) (m2 * a1 + a2)


instance Semigroup PosChange where
    (<>) = combinePosChange


instance Monoid PosChange where
    mempty = PosChange 1 0


totalPosChange :: [Shuffle] -> PosChange
totalPosChange = foldMap findChange


invertChange :: Integer -> PosChange -> PosChange
-- the inverse of \k -> m*k + a is \k -> n*k + b, where n is the multiplicative inverse of m,
-- and b is -n*a
invertChange mod (PosChange m a)
    = let inverse = inverseCoprimes m mod
      in PosChange inverse (- inverse * a)


-- this function, as the name suggests, is hard-coded to compute - in an efficient way -
-- what happens to a transformation when it is repeated exactly 101741582076661 times, for
-- a given base (length of deck).
-- It is based on writing 101741582076661 as 2381 * 39420 * 1083983 + 1
repeatChangeHardCoded :: Integer -> PosChange -> PosChange
repeatChangeHardCoded base ch = ch <> doItNTimes 2381 (doItNTimes 39420 (doItNTimes 1083983 ch))
    where doItNTimes n = reduceChange base . stimes n


solvePart2 :: [Shuffle] -> Integer
solvePart2 ss = applyChange base (invertChange base . repeatChangeHardCoded base
                                    . reduceChange base $ totalPosChange ss) 2020
    where base = 119315717514047


part2 :: IO Integer
part2 = puzzleData >>= return . solvePart2
