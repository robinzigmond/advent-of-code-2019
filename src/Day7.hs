{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Monad.State
import Data.List (permutations, foldl', sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!), (//))

import Day5 (parseFile, Mode(..), Pos, Output, Instruction(..), getInput, getInstruction)


puzzleData :: IO (Vector Int)
puzzleData = TIO.readFile "input/input7.txt" >>= return . parseFile


{- going to use a slightly different state and output types from puzzle 5:
the (maybe) output will now be, well, the output, rather than part of the state,
while the state will store, as well as the vector and current position, how many
inputs have been taken, which we need now because there will be 2 different inputs -}
type NumberInputs = Int

type ProgramState = (Vector Int, Pos, NumberInputs)


-- this is largely a copy of the corresponding value from day 5, unfortunately the changes
-- mean it's not possible to simply import it, so much has to be copied even though it
-- hasn't changed
stateMachine :: Int -> Int -> State ProgramState Output
stateMachine setting input = do
    (vect, pos, numInputs) <- get
    case getInstruction (vect ! pos) of
        Add firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput + secondInput)]
            put (newVect, pos + 4, numInputs)
            return Nothing
        Multiply firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput * secondInput)]
            put (newVect, pos + 4, numInputs)
            return Nothing
        Input -> do
            let outputPos = vect ! (pos + 1)
            let theInput = case numInputs of
                    0 -> setting
                    1 -> input
                    _ -> error $ "third input asked for at position " ++ show pos ++ "!"
            let newVect = vect // [(outputPos, theInput)]
            put (newVect, pos + 2, numInputs + 1)
            return Nothing
        Output -> do
            let theOutput = vect ! (vect ! (pos + 1))
            put (vect, pos + 2, numInputs)
            return $ Just theOutput
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            put (vect, newPos, numInputs)
            return Nothing
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput == 0 then secondInput else pos + 3
            put (vect, newPos, numInputs)
            return Nothing
        IfLessThan firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, numInputs)
            return Nothing
        IfEqual firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, numInputs)
            return Nothing
        End -> return Nothing


fullState :: Int -> Int -> State ProgramState Int
fullState setting input = do
    maybeOut <- stateMachine setting input
    if maybeOut /= Nothing
        then return $ fromJust maybeOut
        else fullState setting input


runProgram :: Vector Int -> Int -> Int -> Int
runProgram v first second = evalState (fullState first second) (v, 0, 0)


-- this function finds the output from running a list of machines in a row
-- (all with the same program), for a given list of phase settings
runAll :: Vector Int -> [Int] -> Int
runAll v = foldl' (flip $ runProgram v) 0


solvePart1 :: Vector Int -> Int
solvePart1 v = maximum . map (runAll v) $ permutations [0..4]


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


-- now for part 2 we're going to have to rewrite the state logic in yet another way :/
-- The program state will also include both the current input (if any)
-- and the current output (if any). The output type will simply indicate
-- whether the program is running happily, waiting for further input,
-- or finished

type Input = Maybe Int

type ProgramState2 = (Vector Int, Pos, Input, Output)

data RunState = Running | Waiting | Finished deriving (Eq)


stateMachine2 :: State ProgramState2 RunState
stateMachine2 = do
    (vect, pos, maybeIn, maybeOut) <- get
    case getInstruction (vect ! pos) of
        Add firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput + secondInput)]
            put (newVect, pos + 4, maybeIn, maybeOut)
            return Running
        Multiply firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput * secondInput)]
            put (newVect, pos + 4, maybeIn, maybeOut)
            return Running
        Input -> case maybeIn of
                     Nothing -> return Waiting
                     Just input -> do
                        let outputPos = vect ! (pos + 1)
                        let newVect = vect // [(outputPos, input)]
                        put (newVect, pos + 2, Nothing, maybeOut)
                        return Running
        Output -> do
            let theOutput = vect ! (vect ! (pos + 1))
            put (vect, pos + 2, maybeIn, Just theOutput)
            return Running
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            put (vect, newPos, maybeIn, maybeOut)
            return Running
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput == 0 then secondInput else pos + 3
            put (vect, newPos, maybeIn, maybeOut)
            return Running
        IfLessThan firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, maybeIn, maybeOut)
            return Running
        IfEqual firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, maybeIn, maybeOut)
            return Running
        End -> return Finished


-- feeds input into a program that's currently waiting, and returns its next output
proceedWithInput :: Int -> State ProgramState2 (Maybe Int)
proceedWithInput input = go False
    where go alreadyInput = do
            runstate <- stateMachine2
            (v, pos, maybeIn, maybeOut) <- get
            case runstate of
                Running -> go alreadyInput
                Waiting -> case alreadyInput of
                    True -> return maybeOut
                    False -> do
                        put (v, pos, Just input, maybeOut)
                        go True
                Finished -> return maybeOut


-- now we'll start the hard work of "wiring up" these machines in a feedback loop. Note that,
-- since we must keep the state of all 5 machines separate, we need an overall state that's a
-- 5-tuple of the individual program states
type StateOfAll = (ProgramState2, ProgramState2, ProgramState2, ProgramState2, ProgramState2)


runEverything :: (Int, Int, Int, Int, Int) -> Int -> State StateOfAll Int
runEverything settings initial = do
    (st1, st2, st3, st4, st5) <- get
    let (s1, s2, s3, s4, s5) = settings
    let (_, newState1) = runState (proceedWithInput s1) st1
    let (_, newState2) = runState (proceedWithInput s2) st2
    let (_, newState3) = runState (proceedWithInput s3) st3
    let (_, newState4) = runState (proceedWithInput s4) st4
    let (_, newState5) = runState (proceedWithInput s5) st5
    put (newState1, newState2, newState3, newState4, newState5)
    feedbackLoop initial

    where feedbackLoop :: Int -> State StateOfAll Int
          feedbackLoop o = do
            (st1, st2, st3, st4, st5) <- get
            if isFinished st5
                then return $ let (_, _, _, Just answer) = execState stateMachine2 st5 in answer
                else do
                    let (Just output1, newState1) = runState (proceedWithInput o) st1 
                    let (Just output2, newState2) = runState (proceedWithInput output1) st2
                    let (Just output3, newState3) = runState (proceedWithInput output2) st3
                    let (Just output4, newState4) = runState (proceedWithInput output3) st4
                    let (Just output5, newState5) = runState (proceedWithInput output4) st5
                    put (newState1, newState2, newState3, newState4, newState5)
                    feedbackLoop output5

          isFinished = (== Finished) . evalState stateMachine2


solvePart2 :: Vector Int -> Int
solvePart2 v = maximum . map (runAll v) $ permutations [5..9]
                    where runAll v perm = evalState (runEverything (toTuple perm) 0) initState
                          toTuple [a, b, c, d, e] = (a, b, c, d, e)
                          initState = (singleInitState, singleInitState, singleInitState, singleInitState, singleInitState)
                          singleInitState = (v, 0, Nothing, Nothing)


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
