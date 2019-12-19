{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)


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


-- fitsInside x y n tells us whether the nxn square with upper-left corner at point (x, y)
-- fits entirely inside the area pulled by the beam.
-- It also returns a (Maybe) location indicating where the test failed, which we need
-- in order to solve the puzzle without repeating a bunch of unnecessary work.
fitsInside :: Vector Integer -> Int -> Int -> Int -> (Bool, Maybe (Int, Int))
fitsInside v x y n = case filter (\(x, y) -> not $ feelsPull v x y)
                             [(toInteger x', toInteger y') | x' <- [x..x+n-1], y' <- [y..y+n-1]] of
                            []    -> (True, Nothing)
                            (l:_) -> (False, let (x0, y0) = l in Just (fromInteger x0, fromInteger y0))


-- this function goes through a list of locations, testing for whether the relevant square fits
-- there, and returns a set of all points which are ruled out as a solution based on "non-pulled"
-- locations found in the check
toExclude :: Vector Integer -> Int -> [(Int, Int)] -> Set (Int, Int)
toExclude v n ls = S.fromList $ concatMap getExclusions ls
    where getExclusions (x, y) = case snd (fitsInside v x y n) of
            Nothing -> []
            Just (x0, y0) -> [(x, y) | x <- [x0 - n + 1..x0], y <- [y0 - n + 1..y0]]


-- still runs too slowly (and therefore I haven't solved it), despite the "optimisations" above
solvePart2 :: Vector Integer -> Int
solvePart2 v = calculate $ go 0 S.empty
        where go n exclude = let toTest = diag exclude n
                             in case filter (uncurry fits) toTest of
                                    [] -> go (n + 1) . S.union exclude $ toExclude v 100 toTest
                                    (l:_) -> l
              fits x y = fst $ fitsInside v x y 100
              calculate (x, y) = 10000 * x + y
              -- test in a "diagonal" pattern
              diag exclude n = [(x, n - x) | x <- [0..n], (x, n - x) `S.notMember` exclude]


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
