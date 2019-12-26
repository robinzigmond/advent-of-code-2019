{-# LANGUAGE TemplateHaskell #-}

module Day21 where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lens.Micro.Platform

import Day9 (parseFile, Pos, RelativeBase, Output, Instruction(..),
                getInstruction, getInput, getOutputPos, updateVector, readVector)


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input21.txt" >>= return . parseFile


data ProgramState = ProgramState {
    _program :: !(Vector Integer),
    _programPosition :: !Pos,
    _relativeBase :: !RelativeBase,
    _currentOutput :: !Output,
    _inputs :: ![Integer]
}


startingState :: Vector Integer -> ProgramState
startingState v = ProgramState {
    _program = v,
    _programPosition = 0,
    _relativeBase = 0,
    _currentOutput = Nothing,
    _inputs = []
}


makeLenses ''ProgramState


data RunState = Running | Waiting | Finished deriving (Eq)


stateMachine :: State ProgramState RunState
stateMachine = do
    vect <- use program
    pos <- use programPosition
    relative <- use relativeBase
    inputList <- use inputs
    case getInstruction (readVector pos vect) of
        Add firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput + secondInput) vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return Running
        Multiply firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput * secondInput) vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return Running
        Input mode -> do
            case inputList of
                [] -> return Waiting
                (i:is) -> do
                    let outputPos = getOutputPos vect relative mode $ pos + 1
                    let newVect = updateVector outputPos i vect
                    program .= newVect
                    currentOutput .=  Nothing
                    programPosition += 2
                    inputs .= is
                    return Running
        Output mode -> do
            let theOutput = getInput vect relative mode $ readVector (pos + 1) vect
            currentOutput .= Just theOutput
            programPosition += 2
            return Running
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            currentOutput .=  Nothing
            programPosition .= newPos
            return Running
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput == 0 then secondInput else pos + 3
            currentOutput .=  Nothing
            programPosition .= newPos
            return Running
        IfLessThan firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return Running
        IfEqual firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            program .= newVect
            currentOutput .=  Nothing
            programPosition += 4
            return Running
        UpdateRelative mode -> do
            let theInput = getInput vect relative mode $ readVector (pos + 1) vect
            relativeBase += theInput
            currentOutput .=  Nothing
            programPosition += 2
            return Running
        End -> return Finished


-- this function takes a list of inputs, runs the machine until it's waiting for more input,
-- then feeds in the new inputs. Returns an output, if any, as well as an indication of
-- whether the program is finished
proceed :: [Integer] -> State ProgramState (Bool, [Integer])
proceed inputList = do
    inputs .= inputList
    go []
    where go cum = do
            status <- stateMachine
            maybeOut <- use currentOutput
            let outList = case maybeOut of
                    Nothing -> cum
                    Just o -> o : cum
            case status of
                Running -> go $! outList
                Finished -> return (True, outList)
                Waiting -> do
                    inputsLeft <- use inputs
                    case inputsLeft of
                        [] -> return (False, outList)
                        _ -> go $! outList


runMachineWithIO :: IO ()
-- runs the machine, prompting for (text) input when required and rendering output as text, using
-- ASCII encoding/decoding. Note that it's been much corrected from the initial version, and still
-- doesn't behave quite correctly: notably, after prompting with "Input instructions:" once, after
-- the input it again prints "Input instructions:" before computing the output. But it works well
-- enough for me to not feel compelled to debug it!
runMachineWithIO = puzzleData >>= go "" "" . startingState
    where go inputStr outputStr st = do
            case map ord outputStr of
                []   -> return ()
                [10] -> return ()
                _   -> putStr $ reverse outputStr
            let ((done, outList), newState) = runState (proceed
                    $ map (pipeForNewLine . toInteger . ord) inputStr) st
            case outList of
                [] -> do
                    input <- getLine
                    if done
                        then putStrLn "program complete!"
                        else go input outputStr newState
                os -> let (asc, nonAsc) = break (>= 128) os
                      in case nonAsc of
                        [] -> go inputStr (map (chr . fromInteger) os) newState
                        (n:_) -> do
                            putStr . reverse $ map (chr . fromInteger) asc
                            putStrLn "got a non-ASCII output:"
                            print n
            if done then putStrLn "program complete!" else return ()
          -- make sure we can use pipe character instead of newline,
          -- just for convenience when interactively running
          pipeForNewLine 124 = 10
          pipeForNewLine n   = n


{-
the winning strategy will consist of the following:
Jump whenever:
- the space 4 in front is solid and one of the next 3 is empty

the relevant program should be:
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK

This indeed works and produces the correct answer: let's first write some code to generate it with
no further input needed
-}

-- note that this will crash with an error unless provided with a "winning" program!
proceedWithFixedProgram :: Vector Integer -> String -> Integer
proceedWithFixedProgram v s =
    let ((_, outputList)) = evalState (proceed
            $ map (toInteger . ord) s) $ startingState v
        (_, (ans:_)) = break (>= 128) outputList
    in ans


correctProg :: String
correctProg = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n"


solvePart1 :: Vector Integer -> Integer
solvePart1 v = proceedWithFixedProgram v correctProg


part1 :: IO Integer
part1 = puzzleData >>= return . solvePart1

{-
For part 2, we find that using the above program with RUN instead of WALK ends up dying when the ground
has the following configuration:

####.#.##.#.####
ABCDEFGHIJKLMNOP

For our program, the droid jumps when it's at B, lands at F, and then can't jump (it would land
at J, which is unoccupied), so walks into the hole at G.

The correct way through this section is obvious: wait till D before jumping, to land at H, then step to I
and jump (landing safely at M). Note that, once a safe landing at H has been made, the rest follows from
the logic of the previous program. The key is to look further ahead to realise it shouldn't jump at B.

One naive attempt - which gets through the above although may not for other layouts - is to avoid jumping
if the positions 5 and 8 ahead are both empty. This is coded as follows:

NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT E T
NOT T T (these 2 lines ensure T holds the same as E)
OR H T (now T holds the OR of E and H)
AND T J (now J will only say to jump if either E or H is occupied)
RUN

And this worked, despite my feeling it's not general enough - and the answer was correct. So let's automate it
as we did for part 1

NOTE: this performed VERY badly to start with: well over 5 minutes, and with memory leak(s) killing my
machine's performance while it was running. After adding some more strictness, and compiling with -O2, it
now runs in around 10 seconds!
-}

correctProg2 :: String
correctProg2 = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT E T\nNOT T T\nOR H T\nAND T J\nRUN\n"


solvePart2 :: Vector Integer -> Integer
solvePart2 v = proceedWithFixedProgram v correctProg2


part2 :: IO Integer
part2 = puzzleData >>= return . solvePart2
