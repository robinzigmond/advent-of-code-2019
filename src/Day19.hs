{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Monad.State
import Data.List (intersect, foldl1')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)

import Debug.Trace (traceShow)

puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input19.txt" >>= return . parseFile


data ProgramState = ProgramState {
    _program :: Vector Integer,
    _programPosition :: Pos,
    _relativeBase :: RelativeBase,
    _currentOutput :: Output,
    _numberInputs :: Int
}


startingState :: Vector Integer -> ProgramState
startingState v = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _numberInputs = 0
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
            numberInputs += 1
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


-- Feed 2 inputs to the Intcode Program, and get the output
isPulled :: Integer -> Integer -> State ProgramState Bool
isPulled x y = do
    inputsUsed <- use numberInputs
    let nextInput = case inputsUsed of
            0 -> x
            1 -> y
            _ -> error "it's had 2 inputs and still asking for more!"
    (_, maybeOut) <- stateMachine nextInput
    case maybeOut of
        Nothing -> isPulled x y
        Just 0  -> return False
        Just 1  -> return True
        _       -> error "got an unexpected output"


feelsPull :: Vector Integer -> Integer -> Integer -> Bool
feelsPull v x y = evalState (isPulled x y) $ startingState v


-- this function uses the above to give a count of how many locations from a given
-- pair of ranges (one for the x co-ordinate and one for the y) return a True result for
-- the above test
countPulled :: [Int] -> [Int] -> Vector Integer -> Int
countPulled xs ys v = length $ filter (uncurry $ feelsPull v)
                        [(toInteger x, toInteger y) | x <- xs, y <- ys]


solvePart1 :: Vector Integer -> Int
solvePart1 = countPulled [0..49] [0..49]


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


{- For the solution to Part 2, we make some assumptions that, although not explicitly stated,
apply to the examples given in the puzzle, and will make the program both simpler and a lot
quicker if we assume them. These are:
1. In each row, only a single contiguous block of locations are pulled.
2. These blocks never decrease in length as you move down
3. Likewise, the position of the left-most location that is pulled never moves backward (to the left)
-}

-- THIS SOLUTION STILL HAS TERRIBLE PERFORMANCE, EVEN CALCULATING PulledInRow TAKES FOREVER!

-- the following function takes a row and returns a pair consisting of the position of the leftmost
-- location that is pulled, and the number of consecutive blocks that are pulled.
-- It also takes an offset to say where to start looking, this is to optimise when looking at
-- several rows one after the other, and is based on one of the assumptions listed above.
pulledInRow :: Vector Integer -> Int -> Int -> (Int, Int)
pulledInRow v offset r = (start, len)
    where isValid x = feelsPull v (toInteger x) $ toInteger r
          pulled = takeWhile isValid $ dropWhile (not . isValid) [offset..3*r]
          -- arbitrary upper limit to stop infinite loop when the row is empty
          start = head pulled
          len = length pulled


-- this function takes a row number and a size, and returns the list of possible column numbers
-- where n consecutive locations, starting from there, are all pulled
possibleStarts :: Vector Integer -> Int -> Int -> Int -> [Int]
possibleStarts v r width offset = let (start, len) = pulledInRow v offset r in [start..start + len - width]


-- this is the key function that takes the size of a square to fit and solve the puzzle for
-- such a square. We use an arbitrary, but almost certainly valid, lower limit of 100,
-- both to (barely noticeably) speed up the function, as well as to avoid problems with some
-- early rows in which nothing is pulled.
closestFit :: Vector Integer -> Int -> (Int, Int)
closestFit v n = go 100
    where nextOffset r = case possibleInRow (r - 1) of
            [] -> 0
            (c:_) -> c
          possibleInRow 100 = possibleStarts v 100 n 0
          possibleInRow r = traceShow r $ possibleStarts v r n $! nextOffset r
          go m = case foldl1' intersect (map possibleInRow [m..m+n-1]) of
                    [] -> go $ m + 1
                    (c:_) -> (c, m)


solvePart2 :: Vector Integer -> Int
solvePart2 v = let (x, y) = closestFit v 100 in 10000 * x + y


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
