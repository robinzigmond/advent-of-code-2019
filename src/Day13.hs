{-# LANGUAGE TemplateHaskell #-}

module Day13 where

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


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input13.txt" >>= return . parseFile


data Tile = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq, Show)


type Location = (Int, Int)

type Game = Map Location Tile


data ProgramState = ProgramState {
    _program :: Vector Integer,
    _programPosition :: Pos,
    _relativeBase :: RelativeBase,
    _currentOutput :: Output,
    _gameState :: Game,
    _gameScore :: Int,
    _joystickPositions :: [Int]
}


startingState :: Vector Integer -> ProgramState
startingState v = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _gameState = M.empty,
    _gameScore = 0,
    _joystickPositions = []
}


makeLenses ''ProgramState


-- this is a literal, unchanged, copy-and-paste from the same value in the Day 11 solution :(
-- Unfortunately it cannot simply be imported because the ProgramState has changed, as therefore
-- have the lenses, even though the ones used go by the same names.
-- (The above was true for Part 1, it has been updated for part 2 to handle the list of joystick
-- inputs as part of the state, as well as removing the input parameter which is now no longer needed. It's
-- still over 90% unchanged though.)
stateMachine :: State ProgramState (Bool, Maybe Integer)
stateMachine = do
    vect <- use program
    pos <- use programPosition
    relative <- use relativeBase
    inputList <- use joystickPositions
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
            case inputList of
                [] -> error "ran out of joystick inputs!"
                (i:is) -> do
                    let outputPos = getOutputPos vect relative mode $ pos + 1
                    let newVect = updateVector outputPos (toInteger i) vect
                    program .= newVect
                    currentOutput .=  Nothing
                    programPosition += 2
                    joystickPositions .= is
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


-- now we need to run this until we have 3 outputs, and use them to make the necessary
-- insertion into the Game map. Note that there appear to be no "input" instructions this time.
addTile :: State ProgramState Bool
addTile = go Nothing Nothing Nothing
    where go maybeX maybeY maybeTile = do
            (isComplete, maybeOut) <- stateMachine
            if isComplete
                then return True
                else case maybeOut of
                        Nothing -> addTile
                        Just output -> case maybeX of
                            Nothing -> go (Just output) maybeY maybeTile
                            Just x -> case maybeY of
                                Nothing -> go maybeX (Just output) maybeTile
                                Just y -> case maybeTile of
                                    Nothing -> do
                                        gameState %= M.insert (fromInteger x, fromInteger y)
                                            (toEnum $ fromInteger output)
                                        return False
                                    Just _ -> error "something went wrong when running the machine!"


fullGame :: State ProgramState Game
fullGame = do
    isDone <- addTile
    if isDone
        then use gameState >>= return
        else fullGame


solvePart1 :: Vector Integer -> Int
solvePart1 = M.size . M.filter (== Block) . evalState fullGame . startingState


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


playForFree :: Vector Integer -> Vector Integer
playForFree v = v // [(0, 2)]


-- modification of the addTile function to take account of the score now
playGame :: State ProgramState Bool
playGame = go Nothing Nothing Nothing
    where go maybeX maybeY maybeTile = do
            (isComplete, maybeOut) <- stateMachine
            if isComplete
                then return True 
                else case maybeOut of
                        Nothing -> playGame 
                        Just output -> case maybeX of
                            Nothing -> go (Just output) maybeY maybeTile
                            Just x -> case maybeY of
                                Nothing -> go maybeX (Just output) maybeTile
                                Just y -> case maybeTile of
                                    Nothing -> if x == -1 && y == 0
                                        then do
                                            gameScore .= fromInteger output
                                            return False
                                        else do
                                            gameState %= M.insert (fromInteger x, fromInteger y)
                                                (toEnum $ fromInteger output)
                                            return False
                                    Just _ -> error "something went wrong when running the machine!"


-- this state value returns, as well as an Int representing the score, a boolean saying if all blocks
-- were destroyed or not
fullGameWithScore :: State ProgramState (Bool, Int)
fullGameWithScore = do
    isDone <- playGame
    if isDone
        then do
            score <- use gameScore
            game <- use gameState
            let blocksLeft = M.size $ M.filter (== Block) game
            return (blocksLeft == 0, score)
        else fullGameWithScore


-- this is the key state-updating function, after spending some time messing with the inner workings and
-- realising this was *actually* playing breakout. We make sure we always move the joystick such that the
-- paddle is level with the ball (probably not the quickest win, but it does work!)
playToWin :: State ProgramState Int
playToWin = do
    isDone <- playGame
    score <- use gameScore
    game <- use gameState
    if isDone
        then return score
        else do
            let blocksLeft = M.size $ M.filter (== Block) game
            let hasBall = not . M.null $ M.filter (== Ball) game
            let hasPaddle = not . M.null $ M.filter (== Paddle) game
            let ballXPos = if hasBall then fst . head . M.keys $ M.filter (== Ball) game else 0
            let paddleXPos = if hasPaddle then fst . head . M.keys $ M.filter (== Paddle) game else 0
            case compare paddleXPos ballXPos of
                LT -> joystickPositions .= [1]
                EQ -> joystickPositions .= [0]
                GT -> joystickPositions .= [-1]
            playToWin


solvePart2 :: Vector Integer -> Int
solvePart2 = evalState playToWin . startingState . playForFree


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
