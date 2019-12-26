module Day25 where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile)
import Day21 (startingState, proceed)


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input25.txt" >>= return . parseFile


-- copy of the corresponding Day 21 value - but can't import because puzzleData is now different!
runMachineWithIO :: IO ()
runMachineWithIO = puzzleData >>= go "" "" . startingState
    where go inputStr outputStr st = do
            case map ord outputStr of
                []   -> return ()
                _   -> putStr $ reverse outputStr
            let ((done, outList), newState) = runState (proceed
                    $ map (pipeForNewLine . toInteger . ord) inputStr) st
            case outList of
                [] -> do
                    input <- getLine
                    go input "" newState
                os -> let (asc, nonAsc) = break (>= 128) os
                      in case nonAsc of
                        [] -> go "" (map (chr . fromInteger) os) newState
                        (n:_) -> do
                            putStr . reverse $ map (chr . fromInteger) asc
                            putStrLn "got a non-ASCII output:"
                            print n
            if done then putStrLn "program complete!" else return ()
          -- make sure we can use pipe character instead of newline,
          -- just for convenience when interactively running
          pipeForNewLine 124 = 10
          pipeForNewLine n   = n



{- So we can play the game now.
Here is the map, together with some notes as to what's there:



          SECU-   SCIENCE
          RITY    LAB
          check   Proto-
          point - type
          Pres-   polymer
          sens.   design
          floor   antenna
          next      
          room
            |       |
          The     CREW
          Floor   Q'S
                  Beds
                  too
                  small
                  Weather
                  machine
                    |
                  HOLO-
                  DECK
                  Giant
                  Grid
                  setting
                  Holo-
                  gram
                    |
  GIFT    ENGI-   ARCADE  OBSERV 
  WRAP  - NEER- - No pow. ATORY
  CTR     ing     Molten  Fuel
  Escape  White   lavaX   Cell
  PodX    board
                    |       |
                  CORRID. SICK
                  Walls/  BAY
                  Floor   Infin.
                  diff.   LoopX
                  colours
                  mani-
                  fold
                    |       |
                  WARP    HALL-
                  DRIVE   WAY
                  MAINT. _astro-
                  Easter  labe
                  Egg
                    |       |
                  START-* NAV.
                          broch-
                          ure
                    |       |
          PASS-   STORE_  HOT
          AGES  -         CHOC
          phot-           FOUNT.
          onsX
           |
          STAB-
          LES
          giant
          elec-
          trom-
          agnetX

*KITCHEN

14 different items, so 2^14 possibilities (around 16,000). The only sensible way has to be automate the process:
- walk through all rooms and pick up everything
- go to the security checkpoint.
- try every combination of dropping/picking up everything, followed by walking through the checkpoint,
watch for the words that say you've failed, and stop the program when they fail to appear, and output
the items carried.

Well actually, from manual playing, it's easy to find that certain items basically end the game when you try
to pick them up: marked with an X above.

There are 8 others - but in most cases you are too heavy. Trying with just a small number of items:

None - too light

Brochure - too light
Astrolabe - too light
Easter Egg - too light
Fuel Cell - too light
Antenna - too light
Weather Machine - too light

Manifold - too heavy
Hologram - too heavy

That doesn't rule out a lot - it just shows that the Hologram can't be taken. So there are 6 usable items,
so 2^6 = 64 possibilities. Still perhaps too many for a complete manual test (although close!),
but we might find out more by a few more tests.

brochure + astrolabe - too light
brochure + easter egg - too light
brochure + fuel cell - too light
brochure + antenna - too light
brochure + weather machine - too light

as an experiment, let's try all 6 of these items - nope, too heavy.
So continue systematically:

astrolabe + easter egg - too light
astrolabe + fuel cell - too light
astrolabe + antenna - too light
astrolabe + weather machine - too light

perhaps we should try 5 of the 6 next?

all except brochure - too light
that means we do need to drop something, but their total weight has to be lighter than the brochure.
It's not clear if the weights are realistic though. Continue systematically through 5/6:

all except astrolabe - too light
all except easter egg - too heavy
all except fuel cell - too heavy
all except antenna - too heavy
all except weather machine - too light

this means we can at least divide the 6 items into 2 categories:
"light" items: easter egg, fuel cell, antenna
"heavy" items: brochure, astrolabe, weather machine

we'll next try leaving off 2 of the 6 (15 possibilities), to see what we find. First, leaving off 2
"light" items:

all except easter egg + fuel cell - oh, that actually worked!

See answer hard-coded below...
-}

part1 :: Int
part1 = 229384
