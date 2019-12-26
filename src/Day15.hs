-- WARNING: this file is LONG, but, mainly in the second half,
-- it's mostly comments and discussion with only scraps of code

{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Control.Monad (when)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)


import Debug.Trace (traceShow, trace)


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input15.txt" >>= return . parseFile


data Position = Empty | Wall | Oxygen deriving (Eq, Show)

type Location = (Int, Int)

type Area = Map Location Position


data ProgramState = ProgramState {
    _program :: Vector Integer,
    _programPosition :: Pos,
    _relativeBase :: RelativeBase,
    _currentOutput :: Output,
    _droidPosition :: Location,
    _knownSpace :: Area,
    _oxygenPosition :: Maybe Location
}


startingState :: Vector Integer -> ProgramState
startingState v = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _droidPosition = (0, 0),
    _knownSpace = M.singleton (0, 0) Empty,
    _oxygenPosition = Nothing
}


makeLenses ''ProgramState


-- again a pretty much literal copy of previous versions
stateMachine :: Integer -> State ProgramState (Bool, Maybe Integer)
stateMachine input = do
    vect <- use program
    pos <- use programPosition
    relative <- use relativeBase
    case getInstruction (readVector pos vect) of
        Add firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput + secondInput) vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return (False, Nothing)
        Multiply firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput * secondInput) vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return (False, Nothing)
        Input mode -> do
            let outputPos = getOutputPos vect relative mode $ pos + 1
            let newVect = updateVector outputPos input vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 2
            return (False, Nothing)
        Output mode -> do
            let theOutput = getInput vect relative mode $ readVector (pos + 1) vect
            currentOutput .= Just theOutput
            programPosition += 2
            return (False, Just theOutput)
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            currentOutput .=  Nothing
            programPosition .= newPos
            return (False, Nothing)
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput == 0 then secondInput else pos + 3
            currentOutput .=  Nothing
            programPosition .= newPos
            return (False, Nothing)
        IfLessThan firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return (False, Nothing)
        IfEqual firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return (False, Nothing)
        UpdateRelative mode -> do
            let theInput = getInput vect relative mode $ readVector (pos + 1) vect
            relativeBase += theInput
            currentOutput .=  Nothing
            programPosition += 2
            return (False, Nothing)
        End -> return (True, Nothing)


moveDroid :: Integer -> Location -> Location
moveDroid 1 (x, y) = (x, y + 1)
moveDroid 2 (x, y) = (x, y - 1)
moveDroid 3 (x, y) = (x - 1, y)
moveDroid 4 (x, y) = (x + 1, y)


-- this function flies the droid according to a given set of instructions, and uses the
-- outputs to update the internal state
fly :: [Integer] -> State ProgramState ()
fly [] = return ()
fly route@(dir:dirs) = do
    pos <- use droidPosition
    let nextPos = moveDroid dir pos
    (_, maybeOut) <- stateMachine dir
    case maybeOut of
        Nothing -> fly route
        Just 0 -> do
            -- hit a wall, so record the wall and don't otherwise change anything
            knownSpace %= M.insert nextPos Wall
            fly dirs
        Just 1 -> do
            -- can fly ok, so record the empty space and move the droid there
            knownSpace %= M.insert nextPos Empty
            droidPosition %= moveDroid dir
            fly dirs
        Just 2 -> do
            -- found the oxygen system! Same as 1 except the insertion changes, and
            -- we update our state to mark the known oxygen position
            knownSpace %= M.insert nextPos Oxygen
            droidPosition %= moveDroid dir
            oxygenPosition .= Just nextPos
            fly dirs


-- function to "pretty print" what we know about the area of space
display :: Area -> Location -> [String]
display m droidLoc = map displayRow [yMax, yMax - 1..yMin]
    where locations = M.keys m
          xMin = minimum $ map fst locations
          xMax = maximum $ map fst locations
          yMin = minimum $ map snd locations
          yMax = maximum $ map snd locations
          displayRow y = map showIt $ map (\x -> (x, y)) [xMin..xMax]
          showIt l
            | l == droidLoc = 'D' 
            | otherwise = case M.lookup l m of
                Nothing -> ' '
                Just Empty -> '.'
                Just Wall -> '#'
                Just Oxygen -> '*'


-- and a general function for seeing the result of executing any State value
result :: State ProgramState a -> IO ()
result s = do
    v <- puzzleData
    let endState = execState s (startingState v)
    let area = _knownSpace endState
    let droid = _droidPosition endState
    mapM_ putStrLn $ display area droid


-- this is just for messing about in GHCi to get a feel for things
flightPath :: [Integer] -> IO ()
flightPath = result . fly

{-
A narrow corridor, at least at the start: flightPath [1,4,3,2,4,3,2,4,2,3,2,3,1,2,3,1,2,3,1,3,2,3,2,4,2,3,2,3]
gives the following picture:

      # 
     #.#
  ####.#
 #.....#
 #.#### 
D..#
 ##
-}

-- now let's write some code to fly the droid in such a way as to try to map out the shape of the area

-- first, a shorthand for flying continuously in a given direction until hitting a wall.
-- It returns the distance flown before hitting the wall
goUntilWall :: Integer -> State ProgramState Int
goUntilWall dir = go 0
    where go n = do
            fly [dir]
            pos <- use droidPosition
            let tryingToGo = moveDroid dir pos
            area <- use knownSpace
            case M.lookup tryingToGo area of
                Just Wall -> return $ fromInteger n
                _         -> go $ n + 1


-- simple function to model turning right, which is what we'll do whenever we hit a wall
turnRight :: Integer -> Integer
turnRight 1 = 4
turnRight 2 = 3
turnRight 3 = 1
turnRight 4 = 2


-- we now adopt a simple strategy of starting by going North, then turning right whenever we
-- hit a wall (or left if we can't go right) - until we either reach the oxygen,
-- or end up in a location we already know. This will help us map out the area.
-- As with goUntilWall, it returns the total distance flown. (Which  gives 958, too high - no
-- great surprise as there is surely a shortcut somewhere.)
explore :: State ProgramState Int
explore = go 1 0
    where go dir n = do
            dist <- goUntilWall dir
            oxygen <- use oxygenPosition
            location <- use droidPosition
            area <- use knownSpace
            let newLocation = moveDroid dir location
            if n > 500 -- crude attempt to limit how long we can explore for
                then return n
                else case M.lookup newLocation area of
                    Just Empty -> return n -- we're somewhere we already know (but doesn't work)
                    _ -> case oxygen of
                        Just _ -> do
                            fly [turnRight $ turnRight dir]
                            return n
                        -- (we've found it, no need to continue, but we move backwards
                        -- to ensure we can see the position of the oxygen)
                        Nothing -> let isClear d = M.lookup (moveDroid d location) area /= Just Wall
                                       availableDirs = [turnRight dir,
                                                        turnRight . turnRight $ turnRight dir,
                                                        turnRight $ turnRight dir]
                                       clear = filter isClear availableDirs
                                   in case clear of
                                    [] -> return n -- can't happen, would mean we've flown into a dead end!
                                    (newDir:_) -> go newDir $ n + dist


seeMap :: IO ()
seeMap = result explore


{- running the above now DOES find the oxygen, and gives the following diagram (it originally didn't mark
the start position, I've added it myself, as S). This shows the route the droid took, but clearly there are
a several points where a shortcut may be possible, depending on if walls exist or not.

       #         #
      #...........#
   # # . # #     .
  #...#...#.....#.
   . . # . . # . . #             
   .#.....#.#.....#S#
   . # # # . . # # .
  #...#...#.#.#.....#
   # . # . . # .   #   # #       
    #.....#.....      #...#      
     #   .     #   #   . .       
        #...........#  . .       
     #   #       # . # . .   #
    #.............#.#...#.....#  
 #   . # #     # . . . # # # .
#.....#.........#.#...#.....#...#
 .   # . .     # . # # . . # # . 
 .#*D... .     ...#    .#....... 
 .     # . #   . .     . #     # 
#.........#.....#.    #...#
 #   #   . . # # . #   # .       
    #.....#.#.....#.....#.       
         # . #   # .   . .       
          #...........#...#      
           #       # # # #      


The easiest way to determine the solution now is to use my interactive flightPath function to explore this.

The following is a copy of the diagram filled in with some relevant new information from doing the above:

       #         #
      #...........#
   # # . # ##### .#
  #...#...#.....#.#
   . . # .#. # .#.##             
   .#.....#.#.....#S#
   . # # ##.#. ### .
  #...#...#.#.#.....#
   # . # .#. # .####   # #       
    #.....#.....#     #...#      
     #   . #####   #   . .       
        #...........#  . .       
     #   #       # . # . .   #
    #.............#.#...#.....#  
 #   . # #     # . . . # # # .
#.....#.........#.#...#.....#...#
 .   # . .     # . # # . . # # . 
 .#*D... .     ...#    .#....... 
 .     # . #   . .     . #     # 
#.........#.....#.    #...#
 #   #   . . # # . #   # .       
    #.....#.#.....#.....#.       
         # . #   # .   . .       
          #...........#...#      
           #       # # # #

As you can see, for early parts of the route at least, attempts at a shortcut find a wall in the way.
Based on this, I assumed that there would be no shortcuts, and that I should simply count the steps on
the route above, just eliminating the paths which obviously led to a dead end. Doing this gave me the right
answer! :-)
-}


{-
For Part 2 though, we need the full map, bounded by walls on all sides. (Since part 2 is essentially
calculating the longest distance from the oxygen to any reachable empty point.)

We'll try to automate this process as much as possible, although some manual work will still be involved.

First we'll extract the actual shortest path, as a hardcoded Haskell list
-}

-- actually it's a run-length-encoded version, because only that enables us to do the "lookaround" properly
shortestPath :: [(Integer, Int)]
shortestPath = [(2,2),(3,4),(2,2),(3,4),(1,6),(4,4),(2,2),(4,2),(1,4),(3,10),(2,2),(4,2),(2,2),
                (3,4),(1,2),(3,2),(2,4),(4,2),(2,2),(4,4),(2,2),(4,10),(2,4),(4,2),(1,2),(4,2),
                (1,4),(4,2),(2,4),(4,4),(2,2),(4,2),(2,2),(3,6),(1,2),(3,2),(2,4),(4,2),(2,4),
                (3,2),(1,2),(3,4),(2,2),(3,8),(1,4),(4,4),(1,2),(4,2),(1,4),(3,12),(2,2),(3,4),
                (2,4),(4,8),(1,4),(3,2),(2,2),(3,4)]


convertRLE :: [(a, Int)] -> [a]
convertRLE = concatMap . uncurry $ flip replicate


part1 :: Int
part1 = length $ convertRLE shortestPath


lookAround :: [(Integer, Int)] -> [Integer]
lookAround = concatMap expandAndWander
    where expandAndWander (dir, l) = dir : concatMap wander (replicate (l - 1) dir)
          wander dir = [turnRight dir, turnLeft dir, turnLeft dir, turnRight dir, dir]
          turnLeft = turnRight . turnRight . turnRight
          goBack = turnRight . turnRight


seeMore :: IO ()
seeMore = flightPath $ lookAround shortestPath


{-
This is still far from perfect, but gives us a rather more complete picture of the area:

        #########
       ...........
    # #.#########.#
   ... ... .....#.#
  #.#.###.#.###.#.#
  #.#.....#.#  ... .
  #.# ### #.#   ###.#
   ...    #.#  .....
    #.### #.###.###     #        
     ..... .....       ...
      ###.#########   #.#.#
         ...........  #.#.#
      #############.# #.#.###
     .............#.#... .....
  ###.###########.#.#.# # ###.#
 ..... ...      #.#... ...   ...
#.#####.#.#     #.# # #.#.#####.#
#.#D....#.#    ...    #.#.......
#.#######.# ###.#     #.# #####
 ......... .....       ...
  ####### #.###     ### #.#
          #.#      .....#.#
          #.#######.###.#.#
           .........   ...
            #######     #

I could superimpose the two pictures manually, but it's easy enough to do it in code:
-}


infoSoFar :: Vector Integer -> (Area, Location)
infoSoFar v = (M.union firstAttempt secondAttempt, let Just l = _oxygenPosition foundOxygenState in l)
    where foundOxygenState = execState explore $ startingState v
          firstAttempt = _knownSpace foundOxygenState
          secondAttempt = _knownSpace . execState (fly $ lookAround shortestPath) $ startingState v


seeBiggerMap :: IO ()
seeBiggerMap = do
    v <- puzzleData
    mapM_ putStrLn . uncurry display $ infoSoFar v


{-
This gives this which, while not fully complete, is pretty good:

       ###########
      #...........#
   ####.#########.#
  #...#...#.....#.#
  #.#.###.#.###.#.##
  #.#.....#.#.....#.#
  #.#######.#. ####.#
  #...#...#.#.#.....#
   ##.###.#.###.####   ###
    #.....#.....      #...#      
     ####.##########  #.#.#      
        #...........# #.#.#      
     ##############.###.#.####
    #.............#.#...#.....#  
 ####.###########.#.#.#######.#
#.....#.........#.#...#.....#...#
#.#####.#.#    ##.#####.#.#####.#
#.#*....#.#    ...#   #.#.......
#.#######.#####.#.    #.######## 
#.........#.....#.    #...#
 ########.#.#### . ######.#      
    #.....#.#.....#.....#.#
         ##.#######.###.#.#      
          #...........#...#      
           ######### # ###       

I just used flightpath in GHCi to fill in the missing parts, using the following paths:

flightPath [2,2,3,3,3,3,2,2,4]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,3,2]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,1]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,4,2]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,3]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,4,4,2,4,2,4,2,4,3]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,4,4,2,2,2,3,4,1,4,4,4,4,1,1,3
    3,2,4,2]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,
    3,3,3,2,2,3,3,3,3,2,2,2,2,4,4,4,4,4,4,4,4,2,2,3,2,3,2,3,2,3,2,3,2,2,3,4,4]
flightPath [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,
    2,2,4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,
    1,1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
    2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,
    3,3,3,2,2,3,3,3,3,2,2,2,2,4,4,4,4,4,4,4,4,1,1,1,1,4,4,2,4,2,4,2,4,2,3,3]

with the result:

       ###########
      #...........#
   ####.#########.#
  #...#...#.....#.#
  #.#.###.#.###.#.##
  #.#.....#.#.....#.#
  #.#######.#.#####.#
  #...#...#.#.#.....#
   ##.###.#.###.####   ###
    #.....#.....#     #...#      
     ####.##########  #.#.#      
        #...........# #.#.#      
     ##############.###.#.####
    #.............#.#...#.....#  
 ####.###########.#.#.#######.##
#.....#.........#.#...#.....#...#
#.#####.#.###.###.#####.#.#####.#
#.#*....#.#...#...#...#.#.......#
#.#######.#####.#.###.#.######## 
#.........#.....#.....#...#
 ########.#.#####.#######.#      
    #.....#.#.....#.....#.#
    #.#####.#######.###.#.#      
    ...   #...........#...#      
     #     ########### ###


Clearly there is a region in the South West still unexplored, which I'm assuming is fairly large
(which is why I stopped doing it manually). Going to get the computer to map this one out again, at least
roughly, by getting into the area before calling the explore state action again.
-}

exploreSW :: State ProgramState ()
exploreSW = do
    fly [2,2,3,3,3,3,2,2,3,3,3,3,1,1,1,1,1,1,4,4,4,4,2,2,4,4,1,1,1,1,3,3,3,3,3,3,3,3,3,3,2,2,
        4,4,2,2,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,4,4,4,4,2,2,4,4,4,4,4,4,4,4,4,4,2,2,2,2,4,4,1,
        1,4,4,1,1,1,1,4,4,2,2,2,2,4,4,4,4,2,2,4,4,2,2,4,3,3,3,3,3,3,1,1,3,3,2,2,2,2,4,4,2,2,2,
        2,3,3,1,1,3,3,3,3,2,2,3,3,3,3,3,3,3,3,1,1,1,1,4,4,4,4,1,1,4,4,1,1,1,1,3,3,3,3,3,3,3,3,
        3,3,3,3,2,2,3,3,3,3,2,2,2,2,4,4,4,4,4,4,4,4,2,2,3,3,3,3,2,2,4]
    explore
    return ()


seeSW :: IO ()
seeSW = result exploreSW

{- this works, after introducing the exploration length limit above (before it got into an infinite
loop exploring the same locations), producing:

#   #     # # #   #             
   ...#........D#.....#
 # . . #   # . . .   . #
 ...#...# #...#...# #...# 
 . # # .   . # # #   # .           
#...#.#.   .          #.           
 # . . . # .           #
#...#...#...#
 . # . # . #                       
 .#...#  .
 . . #   .                         
 .#...#  .                         
 . # .   .                         
#...#.....#                        
 . . #   #                         
 .#...#                            
 . # .      
#.#...#  ...........     
 # . #   .         .     
#...#... ... ..... .
 . # . .   . .   . .               
#...#. ..... .   ... .             
 . . .       .       .             
 . . ...     .   .....             
 . .   .     .   .                 
 . .   ..... .....       ...       
 . .       .             . .       
#.#.       ...........   . .       
 # .                 .   . .       
#...#  ............. . ... .....   
 . #   .           . . .       .   
 . .....           . ... ...   ... 
 . .               .     . .     . 
 . .             ...     . .......#
 . .             .       .         
 . ......... .....       ...       
 .         . .             .       
 .     ..... .       ..... .       
 .     .#  # .       .   . .       
#...........#.........   ...       
 #         #


Working from where the oxygen actually is, I only count 126 max distance along the "new" routes - much
less than to the original starting point. So we must be leaving some tunnels unexplored among the maze.

Next step: rewrite to folllow a "keep wall on right" strategy!
Do this from the start, and don't stop when the oxygen has been discovered (but do mark it).
-}