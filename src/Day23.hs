{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Control.Monad (forM, forM_)
import Control.Monad.State
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)


import Debug.Trace (trace, traceShow)

puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input23.txt" >>= return . parseFile


data ProgramState = ProgramState {
    _program :: !(Vector Integer),
    _programPosition :: !Pos,
    _relativeBase :: !RelativeBase,
    _currentOutput :: !Output,
    _inputQueue :: ![Integer],
    _unprocessedOutputs :: ![Integer],
    _idleCount :: !Int,
    _isIdle :: !Bool
}


startingState :: Vector Integer -> Int -> ProgramState
startingState v n = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _inputQueue = [toInteger n],
    _unprocessedOutputs = [],
    _idleCount = 0,
    _isIdle = False
}


makeLenses ''ProgramState


stateMachine :: State ProgramState (Bool, Maybe Integer)
stateMachine = do
    vect <- use program
    pos <- use programPosition
    relative <- use relativeBase
    inputList <- use inputQueue
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
            let nextInput = case inputList of
                                [] -> (-1)
                                (i:_) -> i
            case inputList of
                [] -> do
                    idleCount += 1
                    counter <- use idleCount
                    isIdle .= (counter > 100) -- 100 is arbitrary, hopefully high enough though!
                (i:is) -> do
                    inputQueue .= is
                    idleCount .= 0
                    isIdle .= False
            let newVect = updateVector outputPos nextInput vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 2
            return (False, Nothing)
        Output mode -> do
            let theOutput = getInput vect relative mode $ readVector (pos + 1) vect
            currentOutput .= Just theOutput
            programPosition += 2
            idleCount .= 0
            isIdle .= False
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


-- the list will always have length 50, so strictly speaking should be a tuple - but
-- that would entail a ridiculous amount of repetitive typing, so I'm using a list instead!
type NetworkState = [ProgramState]

startNetworkState :: Vector Integer -> NetworkState
startNetworkState v = map (startingState v) [0..49]


-- we assume that each machine in the network processes simultaneously, at the same speed,
-- so we go over each machine in sequence, processing one Intcode instruction, while making
-- sure outputs are sent to the input queue of the appropriate machine

wholeNetworkStep :: State NetworkState (Maybe Integer)
wholeNetworkStep = do
    allStates <- get
    let allResults = map (runState stateMachine) allStates
    let newStates = map snd allResults
    put newStates
    let allMaybeOuts = V.fromList $ map (snd . fst) allResults
    dests <- forM [0..49] $ \idx -> case allMaybeOuts ! idx of
        Nothing -> return Nothing
        Just o -> do
            ix idx . unprocessedOutputs %= (o:)
            outputs <- use (ix idx . unprocessedOutputs)
            case outputs of
                [y, x, dest] -> do
                    ix idx . unprocessedOutputs .= []
                    if dest < 51
                        then do
                            ix (fromInteger dest) . inputQueue %= (x:) . (y:)
                            return Nothing
                        else if dest == 255
                            then return (Just y)
                            else error $ "unexpected destination address: " ++ show dest
                _ -> return Nothing
    case filter isJust dests of
        [] -> return Nothing
        (jo:_) -> return jo


solvePart1 :: Vector Integer -> Integer
solvePart1 = evalState repeatNetwork . startNetworkState
    where repeatNetwork = do
            maybeAns <- wholeNetworkStep
            case maybeAns of
                Nothing -> repeatNetwork
                Just a -> return a


part1 :: IO Integer
part1 = puzzleData >>= return . solvePart1


-- some adjustments needed for Part 2...

type NATPacket = Maybe (Integer, Integer)

data NetworkStateWithNAT = NetworkStateWithNAT {
    _machines :: ![ProgramState],
    _nat :: !NATPacket
}

makeLenses ''NetworkStateWithNAT


startNetworkStateWithNat :: Vector Integer -> NetworkStateWithNAT
startNetworkStateWithNat v = NetworkStateWithNAT (map (startingState v) [0..49]) Nothing


idleOrNot :: [ProgramState] -> Bool
idleOrNot = all _isIdle


-- as well as adjusting for the NAT, this state value now returns a Bool, which
-- indicates whether the network is idle or not
networkStepWithNat :: State NetworkStateWithNAT Bool
networkStepWithNat = do
    allStates <- use machines
    let allResults = map (runState stateMachine) allStates
    let newStates = map snd allResults
    machines .= newStates
    let allMaybeOuts = V.fromList $ map (snd . fst) allResults
    forM_ [0..49] $ \idx -> case allMaybeOuts ! idx of
        Nothing -> return ()
        Just o -> do
            machines . ix idx . unprocessedOutputs %= (o:)
            outputs <- use (machines . ix idx . unprocessedOutputs)
            case outputs of
                [y, x, dest] -> do
                    machines . ix idx . unprocessedOutputs .= []
                    machines . ix idx . idleCount .= 0
                    machines . ix idx . isIdle .= False
                    if dest < 51
                        then do
                            machines . ix (fromInteger dest) . inputQueue %= (x:) . (y:)
                            machines . ix (fromInteger dest) . idleCount .= 0
                            machines . ix (fromInteger dest) . isIdle .= False
                        else if dest == 255
                            then trace ("sending packet to NAT: " ++ show (x, y)) $ nat .= Just (x, y)
                            else error $ "unexpected destination address: " ++ show dest
                _ -> return ()
    currentStates <- use machines
    return $ idleOrNot currentStates


-- repeats the above till the network is idle, and returns the pair of integers held in the NAT
-- (after sending them back to machine 0's input queue)
repeatTillIdle :: State NetworkStateWithNAT (Integer, Integer)
repeatTillIdle = do
    networkIdle <- networkStepWithNat
    if networkIdle
        then do
            maybeNat <- use nat
            case maybeNat of
                Nothing -> error "nothing in NAT while network idle"
                Just (x, y) -> do
                    machines . ix 0 . inputQueue .= [x, y]
                    forM_ [0..49] $ \idx -> do
                        machines . ix idx . idleCount .= 0
                        machines . ix idx . isIdle .= False
                    return (x, y)
        else repeatTillIdle


getRepeat :: State NetworkStateWithNAT Integer
getRepeat = go Nothing
    where go maybeLastNat = do
            (x, y) <- repeatTillIdle
            case trace ("network idle, NAT sending packet " ++ show (x, y)) maybeLastNat of
                Nothing -> use nat >>= go
                Just (x0, y0) -> if y0 == y
                    then return y
                    else go $! Just (x, y)


solvePart2 :: Vector Integer -> Integer
solvePart2 = evalState getRepeat . startNetworkStateWithNat


-- this performs badly, but eventually gave an answer: unfortunately it was wrong! Will try to debug later...
-- (pesumably it's the definition of being idle? Is my current one too lenient, or too strict. I would guess
-- too lenient.)
-- Now tried debugging in various ways, and no matter what I change I always get the same sequence of
-- inputs from the NAT, always with a repeated 2251799, which isn't the answer...
part2 :: IO Integer
part2 = puzzleData >>= return . solvePart2
