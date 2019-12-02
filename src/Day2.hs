{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T (splitOn, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V


puzzleData :: IO (Vector Int)
puzzleData = TIO.readFile "input/input2.txt" >>= return . parseFile


parseFile :: Text -> Vector Int
parseFile = V.fromList . map (read . T.unpack) . T.splitOn ","


stateMachine :: State (Vector Int, Int) Bool
stateMachine = do
    (vect, pos) <- get
    case (vect ! pos) of
        1 -> do
            put $ (vect // [(vect ! (pos + 3),
                (vect ! (vect ! (pos + 1))) + (vect ! (vect ! (pos + 2))))], pos + 4)
            return False
        2 -> do
            put $ (vect // [(vect ! (pos + 3),
                (vect ! (vect ! (pos + 1))) * (vect ! (vect ! (pos + 2))))], pos + 4)
            return False
        99 -> return True


fullState :: State (Vector Int, Int) Int
fullState = do
    complete <- stateMachine
    if complete
        then do
            (vect, _) <- get
            return $ vect ! 0
        else fullState


useInput :: Int -> Int -> Vector Int -> Vector Int
useInput x y v = v // [(1, x), (2, y)]


restoreGravityAssist :: Vector Int -> Vector Int
restoreGravityAssist = useInput 12 2


solveProgram :: Vector Int -> Int
solveProgram v = evalState fullState (v, 0)


solvePart1 :: Vector Int -> Int
solvePart1 = solveProgram . restoreGravityAssist


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


checkOutput :: Int -> Int -> Int -> Vector Int -> Bool
checkOutput noun verb target v = (solveProgram . useInput noun verb) v == target


solvePart2 :: Vector Int -> Int
solvePart2 v = go 0 0 19690720
    where
        go noun verb target =
            if checkOutput noun verb target v
                then 100 * noun + verb
                else if verb == 99
                    then if noun == 99
                        then error "no input pairs worked"
                        else go (noun + 1) 0 target
                    else go noun (verb + 1) target


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
