{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import Control.Monad.State
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Lens.Micro.Platform


data Position = Position {
    _xPos :: Int,
    _yPos :: Int,
    _zPos :: Int
}


data Velocity = Velocity {
    _xVelocity :: Int,
    _yVelocity :: Int,
    _zVelocity :: Int
}


data Moon = Moon {
    _pos :: Position,
    _velocity :: Velocity
}


type SystemState = (Moon, Moon, Moon, Moon)


makeLenses ''Position
makeLenses ''Velocity
makeLenses ''Moon


puzzleData :: IO SystemState
puzzleData = TIO.readFile "input/input12.txt" >>= return . parseFile


parseFile :: Text -> SystemState
parseFile t = let [m1, m2, m3, m4] = map parseLine (T.lines t) in (m1, m2, m3, m4)


parseLine :: Text -> Moon
parseLine l = Moon pos vel
    where vel = Velocity 0 0 0
          pos = Position xPos yPos zPos
          [textXPos, textYPos, nearlyTextZPos] = map ((!! 1) . T.splitOn "=") $ T.splitOn ", " l
          textZPos = T.dropEnd 1 nearlyTextZPos
          [xPos, yPos, zPos] = map (read . T.unpack) [textXPos, textYPos, textZPos]


applyGravity :: State SystemState ()
applyGravity = do
    moon1X <- use (_1 . pos . xPos)
    moon2X <- use (_2 . pos . xPos)
    moon3X <- use (_3 . pos . xPos)
    moon4X <- use (_4 . pos . xPos)
    moon1Y <- use (_1 . pos . yPos)
    moon2Y <- use (_2 . pos . yPos)
    moon3Y <- use (_3 . pos . yPos)
    moon4Y <- use (_4 . pos . yPos)
    moon1Z <- use (_1 . pos . zPos)
    moon2Z <- use (_2 . pos . zPos)
    moon3Z <- use (_3 . pos . zPos)
    moon4Z <- use (_4 . pos . zPos)
    _1 . velocity . xVelocity += gravityCalc moon1X [moon2X, moon3X, moon4X]
    _1 . velocity . yVelocity += gravityCalc moon1Y [moon2Y, moon3Y, moon4Y]
    _1 . velocity . zVelocity += gravityCalc moon1Z [moon2Z, moon3Z, moon4Z]
    _2 . velocity . xVelocity += gravityCalc moon2X [moon1X, moon3X, moon4X]
    _2 . velocity . yVelocity += gravityCalc moon2Y [moon1Y, moon3Y, moon4Y]
    _2 . velocity . zVelocity += gravityCalc moon2Z [moon1Z, moon3Z, moon4Z]
    _3 . velocity . xVelocity += gravityCalc moon3X [moon1X, moon2X, moon4X]
    _3 . velocity . yVelocity += gravityCalc moon3Y [moon1Y, moon2Y, moon4Y]
    _3 . velocity . zVelocity += gravityCalc moon3Z [moon1Z, moon2Z, moon4Z]
    _4 . velocity . xVelocity += gravityCalc moon4X [moon1X, moon2X, moon3X]
    _4 . velocity . yVelocity += gravityCalc moon4Y [moon1Y, moon2Y, moon3Y]
    _4 . velocity . zVelocity += gravityCalc moon4Z [moon1Z, moon2Z, moon3Z]
        where gravityCalc p ps = length (filter (> p) ps) - length (filter (< p) ps)


moveMoons :: State SystemState ()
moveMoons = do
    moon1VelocityX <- use (_1 . velocity . xVelocity)
    moon1VelocityY <- use (_1 . velocity . yVelocity)
    moon1VelocityZ <- use (_1 . velocity . zVelocity)
    moon2VelocityX <- use (_2 . velocity . xVelocity)
    moon2VelocityY <- use (_2 . velocity . yVelocity)
    moon2VelocityZ <- use (_2 . velocity . zVelocity)
    moon3VelocityX <- use (_3 . velocity . xVelocity)
    moon3VelocityY <- use (_3 . velocity . yVelocity)
    moon3VelocityZ <- use (_3 . velocity . zVelocity)
    moon4VelocityX <- use (_4 . velocity . xVelocity)
    moon4VelocityY <- use (_4 . velocity . yVelocity)
    moon4VelocityZ <- use (_4 . velocity . zVelocity)
    _1 . pos . xPos += moon1VelocityX
    _1 . pos . yPos += moon1VelocityY
    _1 . pos . zPos += moon1VelocityZ
    _2 . pos . xPos += moon2VelocityX
    _2 . pos . yPos += moon2VelocityY
    _2 . pos . zPos += moon2VelocityZ
    _3 . pos . xPos += moon3VelocityX
    _3 . pos . yPos += moon3VelocityY
    _3 . pos . zPos += moon3VelocityZ
    _4 . pos . xPos += moon4VelocityX
    _4 . pos . yPos += moon4VelocityY
    _4 . pos . zPos += moon4VelocityZ


oneStep :: State SystemState ()
oneStep = applyGravity >> moveMoons


updateSystem :: Int -> State SystemState ()
updateSystem n = foldr (>>) (return ()) $ replicate n oneStep


energy :: SystemState -> Int
energy (m1,m2, m3, m4) = sum $ map ((*) <$> potential <*> kinetic) [m1, m2, m3, m4]
    where potential (Moon (Position x y z) _) = sum $ map abs [x, y, z]
          kinetic (Moon _ (Velocity x y z)) = sum $ map abs [x, y, z]


solvePart1 :: SystemState -> Int
solvePart1 = energy . execState (updateSystem 1000)


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


-- split the problem up into the independent X, Y and Z components in order to find the
-- length of the repeating cycle

findRepeat :: Lens' Position Int -> Lens' Velocity Int -> State SystemState Int
findRepeat posLens vLens = do
    moon1X <- use (_1 . pos . posLens)
    moon1VelocityX <- use (_1 . velocity . vLens)
    moon2X <- use (_2 . pos . posLens)
    moon2VelocityX <- use (_2 . velocity . vLens)
    moon3X <- use (_3 . pos . posLens)
    moon3VelocityX <- use (_3 . velocity . vLens)
    moon4X <- use (_4 . pos . posLens)
    moon4VelocityX <- use (_4 . velocity . vLens)
    doCycle 1 [moon1X, moon1VelocityX, moon2X, moon2VelocityX,
                moon3X, moon3VelocityX, moon4X, moon4VelocityX]
        where doCycle n vals = do
                oneStep
                newMoon1X <- use (_1 . pos . posLens)
                newMoon1VelocityX <- use (_1 . velocity . vLens)
                newMoon2X <- use (_2 . pos . posLens)
                newMoon2VelocityX <- use (_2 . velocity . vLens)
                newMoon3X <- use (_3 . pos . posLens)
                newMoon3VelocityX <- use (_3 . velocity . vLens)
                newMoon4X <- use (_4 . pos . posLens)
                newMoon4VelocityX <- use (_4 . velocity . vLens)
                if [newMoon1X, newMoon1VelocityX, newMoon2X, newMoon2VelocityX,
                    newMoon3X, newMoon3VelocityX, newMoon4X, newMoon4VelocityX]
                    == vals
                    then return n
                    else doCycle (n + 1) vals


xRepeat :: State SystemState Int
xRepeat = findRepeat xPos xVelocity


yRepeat :: State SystemState Int
yRepeat = findRepeat yPos yVelocity


zRepeat :: State SystemState Int
zRepeat = findRepeat zPos zVelocity


solvePart2 :: SystemState -> Int
solvePart2 state = lcm x $ lcm y z
    where x = evalState xRepeat state
          y = evalState yRepeat state
          z = evalState zRepeat state


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
