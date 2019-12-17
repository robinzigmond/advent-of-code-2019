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

import Debug.Trace (traceShow, trace)

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
                (i:is) -> trace "reading input now" $ do
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
                                        then traceShow (fromInteger x, fromInteger y, fromInteger output) $ do
                                            gameScore .= fromInteger output
                                            return False
                                        else traceShow (fromInteger x, fromInteger y, toEnum $ fromInteger output :: Tile) $ do
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


-- made to test with GHCi, to see what kind of length of input is needed to guarantee
-- not running out of inputs
solveWithInput :: [Int] -> IO (Bool, Int)
solveWithInput inputs = do
    v <- puzzleData
    let startGame = playForFree v
    let newStartState = (startingState startGame) { _joystickPositions = inputs }
    return $ evalState fullGameWithScore newStartState


-- from running the above with sample inputs, at least some sequences of 9 instructions are not sufficient,
-- but it seems that 10 might suffice. (Note that I've only ever seen 0 for the score, but perhaps one
-- precise sequence will work!)

allSequences :: Int -> [[Int]]
allSequences 0 = []
allSequences 1 = [[-1], [0], [1]]
allSequences n = [((-1):), (0:), (1:)] <*> allSequences (n - 1)


solvePart2 :: Vector Integer -> Int
solvePart2 v = go 0
    where go n =
            let inputs = V.fromList (allSequences 10) ! n
                startGame = playForFree v
                newStartState = (startingState startGame) { _joystickPositions = traceShow inputs inputs }
                ((won, score), endState) = runState fullGameWithScore newStartState
                unused = length $ _joystickPositions endState
                increment = 3 ^ unused -- this isn't fully accurate and may skip some legitimate inputs
            in traceShow score $ if won then score else go (n + increment)


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2

-- making progress now, just by including the above debug statements and watching the output. Things to note:
-- 1) This really does simulate a breakout game!
-- 2) The score does increase when you destroy a block, but it seems to go back to 0 when you "die" - hence
-- always seeing output of 0.
-- 3) if you play accurately there will surely be a LOT more than 10 inputs!
-- 4) Not totally sure how to solve - it could just be a case of observing the positions of things, and what
-- works, and trying to "aim" the ball. Seems too manual, time-consuming and error-prone - but don't see
-- how to code it either. (Other than to compute the score when the correct joystick sequence is known.)

-- getting the hang of it now, maybe? Definitely more than 10 needed!

--  [1,0,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,-1,0,-1] is a promising start, but in order to really
-- figure it out I think I need to look at the starting grid, figure out how the maths works (it's simple
-- for ball and paddle, but I'm not sure yet how the ball comes of the paddle - which depends on either the
-- paddle's recent movement or how far it is from the ball - or off the blocks)
