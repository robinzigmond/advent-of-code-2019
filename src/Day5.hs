{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T (splitOn, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V


puzzleData :: IO (Vector Int)
puzzleData = TIO.readFile "input/input5.txt" >>= return . parseFile


parseFile :: Text -> Vector Int
parseFile = V.fromList . map (read . T.unpack) . T.splitOn ","


data Mode = Position | Immediate deriving (Enum)


type Pos = Int

type Output = Maybe Int

type ProgramState = (Vector Int, Pos, Output)


data Instruction = Add Mode Mode | Multiply Mode Mode | Input | Output
                    | JumpIfTrue Mode Mode | JumpIfFalse Mode Mode
                    | IfLessThan Mode Mode | IfEqual Mode Mode | End


getMode :: Int -> Mode
getMode = toEnum


getInstruction :: Int -> Instruction
getInstruction n =
    let (modes, instr) = n `divMod` 100
        (second, first) = modes `divMod` 10
    in case instr of
        1 -> Add (getMode first) (getMode second)
        2 -> Multiply (getMode first) (getMode second)
        3 -> Input
        4 -> Output
        5 -> JumpIfTrue (getMode first) (getMode second)
        6 -> JumpIfFalse (getMode first) (getMode second)
        7 -> IfLessThan (getMode first) (getMode second)
        8 -> IfEqual (getMode first) (getMode second)
        99 -> End


getInput :: Vector Int -> Mode -> Int -> Int
getInput v Position n = v ! n
getInput v Immediate n = n


stateMachine :: Int -> State ProgramState (Bool, Maybe Int)
stateMachine input = do
    (vect, pos, output) <- get
    case getInstruction (vect ! pos) of
        Add firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput + secondInput)]
            put (newVect, pos + 4, output)
            return (False, output)
        Multiply firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, firstInput * secondInput)]
            put (newVect, pos + 4, output)
            return (False, output)
        Input -> do
            let outputPos = vect ! (pos + 1)
            let newVect = vect // [(outputPos, input)]
            put (newVect, pos + 2, output)
            return (False, output)
        Output -> do
            let theOutput = vect ! (vect ! (pos + 1))
            put (vect, pos + 2, Just theOutput)
            return (False, Just theOutput)
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            put (vect, newPos, output)
            return (False, output)
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newPos = if firstInput == 0 then secondInput else pos + 3
            put (vect, newPos, output)
            return (False, output)
        IfLessThan firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, output)
            return (False, output)
        IfEqual firstMode secondMode -> do
            let firstInput = getInput vect firstMode $ vect ! (pos + 1)
            let secondInput = getInput vect secondMode $ vect ! (pos + 2)
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = vect ! (pos + 3)
            let newVect = vect // [(outputPos, newVal)]
            put (newVect, pos + 4, output)
            return (False, output)
        End -> return (True, output)


fullState :: Int -> State ProgramState Int
fullState input = do
    (complete, _) <- stateMachine input
    if complete
        then do
            (_, _, output) <- get
            return $ fromJust output -- dirty use of fromJust, but should be OK
        else fullState input


solvePart1 :: Vector Int -> Int
solvePart1 v = evalState (fullState 1) (v, 0, Nothing)


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


endWithOutput :: Int -> State ProgramState Int
-- slight modification for part 2, we're told only one output is produced, so to avoid any
-- trickiery where the program runs for ages before reaching a 99 opcode - or perhaps never
-- does! - we will end whenever an output is produced
endWithOutput input = do
    (complete, maybeOut) <- stateMachine input
    if maybeOut /= Nothing
        then return $ fromJust maybeOut
        else endWithOutput input


solvePart2 :: Vector Int -> Int
solvePart2 v = evalState (endWithOutput 5) (v, 0, Nothing)


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
