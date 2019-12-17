module Day16 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Vector as V


puzzleData :: IO [Int]
puzzleData = TIO.readFile "input/input16.txt" >>= return . parseFile


parseFile :: Text -> [Int]
parseFile = map (read . T.unpack) . T.chunksOf 1


phase :: [Int] -> [Int]
phase ns = V.toList . V.imap transform $ V.fromList ns
    where transform ix _ = (`mod` 10) . abs . sum $ zipWith (*) (pattern ix) ns
          pattern i = drop 1 . concatMap (replicate $ i + 1) $ cycle [0, 1, 0, -1]

    
series :: Int -> [Int] -> [Int]
series 1 = phase
series n = phase . series (n - 1)


solvePart1 :: [Int] -> String
solvePart1 = concatMap show . take 8 . series 100


part1 :: IO String
part1 = puzzleData >>= return . solvePart1


{-
For part 2, a naive approach (as above) doesn't work, since we would have to do 100 passes through a list
which now has a length of 6.5 million! No matter which data structure I choose, this just isn't going to
run in any reasonable length of time.

Instead, we'll try to take advantage of the fact that the massive list is just the one list (of length 650),
repeated 10,000 times. (Note: potential spoilers below, if this works - or perhaps even if it doesn't!)

In the transformation of the list ns, the value of the i'th number is given by a "dot product" of 2 vectors.
One is ns itself, the other the list got by replacing each element of [0,1,0,-1] by i copies of itself, then
dropping the first 0.

We note that, because all elements of this second vector are 1,-1 and 0, this is equivalent to just taking
2 sublists, from specific indices, summing them both and subtracting one from the other. Those indices have
the following general formula (note that indices here are counting from 1):

element #    positive indices          negative indices
1            1,5,9,...                 3,7,11,...
             (4n-3, n=1..)             (4n-1, n=1..)
2            2,3,10,11,18,19,...       6,7,14,15,...
             (8n-k, n=1.., k=5 or 6)   (8n-k, n=1.., k=1 or 2)
3            3,4,5,15,16,17...         9,10,11,21,22,23,...
             (12n-k, n=1.., k=7/8/9)   (12n-k, n=1.., k=1,2,3)
             
             
The general pattern is clear. For index i, the positive indices are of the form (given here as a
Haskell list comprehension)

[4*i*n - k | n <- [1..], k <- [2*i + 1..3*i]]

while the negative ones are

[4*i*n - k | n <- [1..], k <- [1..i]]

Does knowing this help us gain performance though? It seems not - while, if we switch the list for a
vector, we can look up indices in O(1), the number of indices to lookup, and then add up the values of,
is proportional to n.

We're still not using the fact that the initial sequence is periodic (with period 650). Can we take advantage
of this? If it's possible to do so, then we will need the output to also be periodic. Is it?

(I can't yet say for sure, although I doubt it. Going to leave this puzzle for at least a little while...)
-}

solvePart2 :: [Int] -> String
solvePart2 ns = concatMap show . take 8 . drop dropNum . series 100 . concat $ replicate 10000 ns
    where dropNum = read . concatMap show $ take 7 ns


part2 :: IO String
part2 = puzzleData >>= return . solvePart2
