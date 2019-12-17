{-# LANGUAGE TemplateHaskell #-}

module Day17 where

import Control.Monad.State
import Data.Char (chr, ord)
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
puzzleData = TIO.readFile "input/input17.txt" >>= return . parseFile


data ProgramState = ProgramState {
    _program :: Vector Integer,
    _programPosition :: Pos,
    _relativeBase :: RelativeBase,
    _currentOutput :: Output,
    _inputs :: [Integer]
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


-- again a pretty much literal copy of previous versions
stateMachine :: State ProgramState (Bool, Maybe Integer)
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
                [] -> error "needed another input which wasn't provided!"
                (i:is) -> do
                    let outputPos = getOutputPos vect relative mode $ pos + 1
                    let newVect = updateVector outputPos i vect
                    program .= newVect
                    currentOutput .=  Nothing
                    programPosition += 2
                    inputs .= is
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


getOutput :: Vector Integer -> String
getOutput = reverse . evalState (go "") . startingState
    where go soFar = do
            (isComplete, maybeOut) <- stateMachine
            if isComplete
                then return soFar
                else case maybeOut of
                        Nothing -> go soFar
                        Just o -> go $ (chr $ fromInteger o) : soFar


seeArea :: IO ()
seeArea = puzzleData >>= putStrLn . getOutput

{-
The above is only there to enable me to see a printout, and confirm everything's working - which it is:

............#############......................................
............#...........#......................................
............#.#############....................................
............#.#.........#.#....................................
............#.#.......#######..................................
............#.#.........#.#.#..................................
............#.#.........#.#.#.......................###########
............#.#.........#.#.#.......................#.........#
..###########.#.........#.#.#.......................#.........#
..#...........#.........#.#.#.......................#.........#
#############.#.........#.#.#.......................#.........#
#.#.........#.#.........#.#.#.......................#.........#
#.#.........#.###########.###########.....###########.........#
#.#.........#...............#.......#.....#...................#
#.#.........#...............###########.###########...........#
#.#.........#.......................#.#.#.#.......#...........#
#.#.........#.......................#.#.#.#.......#...........#
#.#.........#.......................#.#.#.#.......#...........#
#.#.........#.......................#.#.#.#.......#.###########
#.#.........#.......................#.#.#.#.......#.#..........
#.###########.......................#.#.#.#.......#.#..........
#...................................#.#.#.#.......#.#..........
########^...........................#######.......#.#..........
......................................#.#.........#.#..........
......................................#############.#..........
........................................#...........#..........
........................................#############..........

It's possible to answer part 1 "by hand" from that picture, but that's obviously stupid. We can write
code to figure it out, starting by translating the above into a Map of the appropriate type:
-}

type Location = (Int, Int)

data Position = Scaffold | Open | Robot Orientation deriving (Eq)

data Orientation = PointingUp | PointingDown | PointingLeft | PointingRight deriving (Eq)

type Area = Map Location Position


translateChar :: Char -> Position
translateChar '.' = Open
translateChar '#' = Scaffold
translateChar '^' = Robot PointingUp
translateChar 'v' = Robot PointingDown
translateChar '<' = Robot PointingLeft
translateChar '>' = Robot PointingRight
translateChar c = error $ "unknown character encountered: " ++ pure c


getMap :: String -> Area
getMap = foldr M.union M.empty . zipWith insertFunction [0..] . lines
    where insertFunction rowNo = foldr (uncurry M.insert) M.empty
            . zipWith (\colNo c -> ((colNo, rowNo), translateChar c)) [0..]


isIntersection :: Location -> Area -> Bool
isIntersection l m = all (isScaffold m) $ selfAndNeighbours l
    where selfAndNeighbours (x, y) = [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y -1)]
          isScaffold m l = M.lookup l m == Just Scaffold


allIntersections :: Vector Integer -> [Location]
allIntersections v = let area = getMap (getOutput v)
                     in filter (flip isIntersection area) $ M.keys area


alignmentParameter :: Location -> Int
alignmentParameter = uncurry (*)


solvePart1 :: Vector Integer -> Int
solvePart1 = sum . map alignmentParameter . allIntersections


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1

{-
PART 2:

From merely inspecting the diagram above, and tracing the route, we see the full path is the following:

L,8,R,12,R,12,R,10,R,10,R,12,R,10,L,8,R,12,R,12,R,10,R,10,R,12,R,10,L,10,R,10,L,6,
L,10,R,10,L,6,R,10,R,12,R,10,L,8,R,12,R,12,R,10,R,10,R,12,R,10,L,10,R,10,L,6


this suggests the following "programs", which meet the requirements:
A: L,8,R,12,R,12,R,10 (18 chars)
B: R,10,R,12,R,10 (14 chars)
C: L,10,R,10,L,6 (13 chars)
full: A,B,A,B,C,C,B,A,B,C (19 chars)

which we'll use to get the solution!
-}

wakeupProgram :: Vector Integer -> Vector Integer
wakeupProgram v = v // [(0, 2)]


runWithInputString :: String -> Vector Integer -> Integer
runWithInputString s v = evalState fullRun $ (startingState v) {_inputs = asciiValues}
    where asciiValues = map (toInteger . ord) s
          fullRun = do
            (_, maybeOut) <- stateMachine
            stillLeft <- use inputs
            case stillLeft of
                [] -> case maybeOut of
                        Nothing -> fullRun -- keep running till we get the output
                        Just n -> if n < 128 -- keep reading ASCII output, only stop for a "big" one
                                    then fullRun
                                    else return n 
                _ -> fullRun


solvePart2 :: Vector Integer -> Integer
solvePart2 = runWithInputString "A,B,A,B,C,C,B,A,B,C\nL,8,R,12,R,12,R,10\nR,10,R,12,R,10\nL,10,R,10,L,6\nn\n"
                    . wakeupProgram


part2 :: IO Integer
part2 = puzzleData >>= return . solvePart2
