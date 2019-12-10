{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Control.Monad.State
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T (splitOn, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!?), (//))
import qualified Data.Vector as V


puzzleData :: IO (Vector Integer)
puzzleData = TIO.readFile "input/input9.txt" >>= return . parseFile


parseFile :: Text -> Vector Integer
parseFile = V.fromList . map (read . T.unpack) . T.splitOn ","


data Mode = Position | Immediate | Relative deriving (Enum, Show)


type Pos = Integer

type RelativeBase = Integer

type Output = Maybe Integer

type ProgramState = (Vector Integer, Pos, RelativeBase, Output)


data Instruction = Add Mode Mode Mode | Multiply Mode Mode Mode
                    | Input Mode | Output Mode
                    | JumpIfTrue Mode Mode | JumpIfFalse Mode Mode
                    | IfLessThan Mode Mode Mode | IfEqual Mode Mode Mode
                    | UpdateRelative Mode | End deriving (Show)


getMode :: Integer -> Mode
getMode = toEnum . fromInteger


getInstruction :: Integer -> Instruction
getInstruction n =
    let (modes, instr) = n `divMod` 100
        (others, first) = modes `divMod` 10
        (third, second) = others `divMod` 10
    in case instr of
        1 -> Add (getMode first) (getMode second) (getMode third)
        2 -> Multiply (getMode first) (getMode second) (getMode third)
        3 -> Input (getMode first)
        4 -> Output (getMode first)
        5 -> JumpIfTrue (getMode first) (getMode second)
        6 -> JumpIfFalse (getMode first) (getMode second)
        7 -> IfLessThan (getMode first) (getMode second) (getMode third)
        8 -> IfEqual (getMode first) (getMode second) (getMode third)
        9 -> UpdateRelative (getMode first)
        99 -> End


getInput :: Vector Integer -> RelativeBase -> Mode -> Integer -> Integer
getInput v _ Position n = readVector n v
getInput _ _ Immediate n = n
getInput v b Relative n = readVector (b + n) v


getOutputPos :: Vector Integer -> RelativeBase -> Mode -> Integer -> Integer
-- slightly different to the above as output seems to work slightly differently.
-- note that n is now the index, rather than the number read from it
getOutputPos v _ Position n = readVector n v
getOutputPos _ _ Immediate _ = error "Immediate mode for an output parameter - shouldn't happen!"
getOutputPos v b Relative n = readVector n v + b


updateVector :: Integer -> Integer -> Vector Integer -> Vector Integer
-- custom function to update a vector, with an integer that may be out of bounds
updateVector n a v
        | n >= toInteger (V.length v) =
            let lengthDiff = n - toInteger (V.length v)
            in V.concat [v, V.replicate (fromInteger lengthDiff) 0, V.singleton a]
        | otherwise = v // [(fromInteger n, a)]


readVector :: Integer -> Vector Integer -> Integer
-- the same as the above, to safely read from a possibly out-of-bounds index
readVector n v = fromMaybe 0 $ v !? (fromInteger n)


stateMachine :: Integer -> State ProgramState (Bool, Maybe Integer)
stateMachine input = do
    (vect, pos, relative, output) <- get
    case getInstruction (readVector pos vect) of
        Add firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput + secondInput) vect
            put (newVect, pos + 4, relative, output)
            return (False, Nothing)
        Multiply firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos (firstInput * secondInput) vect
            put (newVect, pos + 4, relative, output)
            return (False, Nothing)
        Input mode -> do
            let outputPos = getOutputPos vect relative mode $ pos + 1
            let newVect = updateVector outputPos input vect
            put (newVect, pos + 2, relative, output)
            return (False, Nothing)
        Output mode -> do
            let theOutput = getInput vect relative mode $ readVector (pos + 1) vect
            put (vect, pos + 2, relative, Just theOutput)
            return (False, Just theOutput)
        JumpIfTrue firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput /= 0 then secondInput else pos + 3
            put (vect, newPos, relative, output)
            return (False, Nothing)
        JumpIfFalse firstMode secondMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newPos = if firstInput == 0 then secondInput else pos + 3
            put (vect, newPos, relative, output)
            return (False, Nothing)
        IfLessThan firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput < secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            put (newVect, pos + 4, relative, output)
            return (False, Nothing)
        IfEqual firstMode secondMode thirdMode -> do
            let firstInput = getInput vect relative firstMode $ readVector (pos + 1) vect
            let secondInput = getInput vect relative secondMode $ readVector (pos + 2) vect
            let newVal = if firstInput == secondInput then 1 else 0
            let outputPos = getOutputPos vect relative thirdMode $ pos + 3
            let newVect = updateVector outputPos newVal vect
            put (newVect, pos + 4, relative, output)
            return (False, Nothing)
        UpdateRelative mode -> do
            let theInput = getInput vect relative mode $ readVector (pos + 1) vect
            put (vect, pos + 2, relative + theInput, output)
            return (False, Nothing)
        End -> return (True, Nothing)


fullState :: Integer -> State ProgramState Integer
fullState input = do
    (_, maybeOut) <- stateMachine input
    case maybeOut of
        Just output -> return output
        Nothing -> fullState input


solvePart1 :: Vector Integer -> Integer
solvePart1 v = evalState (fullState 1) (v, 0, 0, Nothing)


part1 :: IO Integer
part1 = puzzleData >>= return . solvePart1


solvePart2 :: Vector Integer -> Integer
solvePart2 v = evalState (fullState 2) (v, 0, 0, Nothing)


part2 :: IO Integer
part2 = puzzleData >>= return . solvePart2
