module Day16 where

import Data.Char (intToDigit)
import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector ((!))
import qualified Data.Vector as V
import Math.Combinatorics.Exact.Binomial (choose)

import Debug.Trace (traceShow)

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
solvePart1 = map intToDigit . take 8 . series 100


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

What's particularly interesting, and relevant, is what happens at the END of the sequence:

- the LAST element of the sequence will always remain the same. This is because, for position N,
the cycle will concist of N-1 0s and a single 1.
- for the same reason, the penultimate element will be the sum of the last 2 elements of the previous version.
(reduced mod 10). Since the last element is fixed, that means that, if the original last 2 elements are a and b,
then the penultimate element is, consecutively: a, a + b, a + 2b, etc. This must repeat every 10 cycles, or
every 2 if b is 5 (or is constant if b is 0)

In our specific input, a is 8 and b is 9. So the sequence of last 2 digits is:
89
79
69
...
09
99
89
and so on, repeating every 10 times. In particular, after 100 repetitions, we're back to 89.

- for the antepenultimate (third from last), the sequence is (for last 3 elements a, b, c):
a, b, c
a + b + c, b + c, c
a + 2b + 3c, b + 2c, c
a + 3b + 6c, b + 3c, c
a + 4b + 10c, b + 4c, c
... at generation n, the 3rd-from-last element will be
a + nb + (n(n + 1)/2)c
mod 10 of course.

at generation 100, reducing modulo 10, we find it is a + 5050c = a + 50c


In general, if the last K elements are, counting from the end, s1...sK, then at generation n we have
that the Kth from last element is (provided of course that K is less than half the length of the sequence)

(n-1)C(n-1)Sk + ... + (k+n-2)C(n-1)s1


- and this is the "magic" formula we need. Unfortunately, it's still a sum of around half a million terms,
and even though many of them are 0 (mod 10), there doesn't seem to be a simple formula for which ones are
or are not, and what the other ones are congruent to mod 10.


But simply repeating the sum 100 times, although fine for the original sequence, is too computationally
expensive for the ~500,000 we actually need. It would be ideal to be able to compute the result without
having to go beyond the original sequence of length 650.

At generation 0 (the start), the "full" sequence of course divides into equal blocks of size 650.

At generation 1, we can still divide it into blocks of 650, but they will no longer be identical.
They will differ by mapping one function over the list though - adding successive multiples of the
sum of the original sequence. (call this sum S0)
At generation 2, the situation becomes more complicated. The second set of 650 (counting back from the
end) will again differ from the first by having the sum (S1) of the first set of 650 added to each element,
but in addition, because of the difference from generation 1, each individual element will have a multiple
of S0 added to it, increasing as you count back from the end.

In other words, each individual element will be of the form a + bS0 + bcS1, where:
a is the corresponding element in the same position in the final set of 650
b is the position in the particular set of 650, counting up from the end
c is the number of the whole set, again counting back from the end (where the last is 0)

- but this isn't particularly nice, and it's not yet clear how it continues in future generations.
-}


phase2 :: [Int] -> [Int]
phase2 = map ((`mod` 10) . sum) . init . tails


series2 :: Int -> [Int] -> [Int]
series2 1 = phase2
series2 n = phase2 . series2 (n - 1)


solvePart2 :: [Int] -> String
solvePart2 ns = map (intToDigit . getResult) [theNumber+1..theNumber+8]
    where theNumber = read . map intToDigit $ take 7 ns
          asVect = V.fromList ns
          getResult 1 = getIdx asVect (-1)
          getResult n = ((((((98 + n) `choose` 99) `mod` 10) * getIdx asVect (-n)) `mod` 10)
                            + traceShow (getResult (n - 1)) getResult (n - 1)) `mod` 10
          getIdx v i = v ! (i `mod` V.length v)


part2 :: IO String
part2 = puzzleData >>= return . solvePart2
