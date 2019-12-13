{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Control.Monad.State
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lens.Micro.Platform


import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input11.txt" >>= return . parseFile


data PaintColour = Black | White deriving (Enum, Eq)

data Direction = FacingUp | FacingRight | FacingDown | FacingLeft deriving (Enum, Bounded, Eq)


turnLeft :: Direction -> Direction
turnLeft d
    | d == minBound = maxBound
    | otherwise = pred d


turnRight :: Direction -> Direction
turnRight d
    | d == maxBound = minBound
    | otherwise = succ d


type Location = (Int, Int)


robotMove :: Direction -> Location -> Location
robotMove FacingUp    (x, y) = (x, y + 1)
robotMove FacingDown  (x, y) = (x, y - 1)
robotMove FacingLeft  (x, y) = (x - 1, y)
robotMove FacingRight (x, y) = (x + 1, y)


type Hull = Map Location PaintColour


panelColour :: Hull -> Location -> PaintColour
panelColour h = fromMaybe Black . (h !?)


data ProgramState = ProgramState {
    _program :: Vector Integer,
    _programPosition :: Pos,
    _relativeBase :: RelativeBase,
    _currentOutput :: Output,
    _robotPosition :: Location,
    _robotDirection :: Direction,
    _hull :: Hull,
    _panelsPainted :: Set Location
}


startingState :: Vector Integer -> ProgramState
startingState v = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _robotPosition = (0, 0),
    _robotDirection = FacingUp,
    _hull = M.empty,
    _panelsPainted = S.empty
}


makeLenses ''ProgramState


-- this is the basic state machine from before, now expressed using lenses as it's more convenient
-- and readable. Although the state contains the hull and robot information, this State value
-- doesn't actually care about that, it simply gives an output (or not).
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


-- this is the function that takes an input and gives a State value that updates the robot
-- which is painting the hull (as well as the hull itself)
paintCycle :: Integer -> State ProgramState Bool
paintCycle input = go False
    where go hasPainted = do
            (isComplete, maybeOut) <- stateMachine input
            if isComplete
                then return True
                else do
                    case maybeOut of
                        Nothing -> go hasPainted
                        Just output -> do
                            if hasPainted
                                then case output of
                                        0 -> do
                                            newDir <- robotDirection <%= turnLeft
                                            robotPosition %= robotMove newDir
                                        1 -> do
                                            newDir <- robotDirection <%= turnRight
                                            robotPosition %= robotMove newDir
                                        _ -> error "got an output that wasn't 0 or 1!"
                                else case output of
                                        0 -> do
                                            pos <- use robotPosition
                                            hull %= M.insert pos Black
                                            panelsPainted %= S.insert pos
                                            go True
                                            return ()
                                        1 -> do
                                            pos <- use robotPosition
                                            hull %= M.insert pos White
                                            panelsPainted %= S.insert pos
                                            go True
                                            return ()
                                        _ -> error "got an output that wasn't 0 or 1!"
                            return False


fullPaint :: State ProgramState (Hull, Set Location)
fullPaint = do
    inProgress <- use hull
    pos <- use robotPosition
    isDone <- paintCycle . toInteger . fromEnum $ panelColour inProgress pos
    if isDone
        then do
            alreadyPainted <- use panelsPainted
            return (inProgress, alreadyPainted)
        else fullPaint


solvePart1 :: Vector Integer -> Int
solvePart1 v = let (_, painted) = evalState fullPaint (startingState v) in S.size painted


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


showHull :: Hull -> [String]
showHull hull
    | xrange > 100 = error "hull is too wide to draw!"
    | yrange > 100 = error "hull is too tall to draw!"
    | otherwise = map drawRow [ymax, ymax-1..ymin]
    where painted = M.keys $ M.filter (== White) hull
          xmin = minimum $ map fst painted
          xmax = maximum $ map fst painted
          ymin = minimum $ map snd painted
          ymax = maximum $ map snd painted
          xrange = xmax - xmin
          yrange = ymax - ymin
          inRow row (_, y) = row == y
          pointsInRow row = filter (inRow row) painted
          drawOneRow row points
            = map (\x -> if (x, row) `elem` points then '#' else ' ') [xmin..xmax]
          drawRow row = drawOneRow row $ pointsInRow row


solvePart2 :: Vector Integer -> [String]
solvePart2 v = showHull completeHull
    where startHull = M.fromList [((0,0), White)]
          startPainted = set hull startHull $ startingState v
          (completeHull, _) = evalState fullPaint startPainted


drawPart2 :: IO [String]
drawPart2 = puzzleData >>= return . solvePart2
