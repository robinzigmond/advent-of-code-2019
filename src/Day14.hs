{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Data.Bifunctor (second)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as M
import Data.List (sortOn)
import Data.Maybe (fromJust, catMaybes, fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)

import Debug.Trace (traceShow)


type IngredientList = HashMap Text Int

-- this type synonym holds information about how to make a given "ingredient".
-- the Int is how many can be made, and the list gives the ingredients necessary
-- to make it, along with the quantities 
type IngredientData = (Int, IngredientList)


type Reactions = HashMap Text IngredientData


puzzleData :: IO Reactions
puzzleData = TIO.readFile "input/input14.txt" >>= return . parseFile


parseFile :: Text -> Reactions
parseFile = foldr (\l m -> M.insert (parseTarget l) (parseIngredients l) m) M.empty . T.lines
    where parseLine :: Text -> (Text, IngredientData)
          parseLine l = let parts = T.splitOn " => " l
                            separate = T.splitOn ", " (parts !! 0)
                            getParts = T.splitOn " "
                            getNumber = read . T.unpack . (!! 0) . getParts
                            getName = (!! 1) . getParts
                            targetInfo = parts !! 1
                            ingredientsInfo = map getParts separate
                        in (getName targetInfo, (getNumber targetInfo,
                                M.fromList $ map (\[n, t] -> (t, read (T.unpack n))) ingredientsInfo))
          parseTarget = fst . parseLine
          parseIngredients= snd . parseLine


mergeIngredients :: IngredientList -> IngredientList -> IngredientList
mergeIngredients = M.unionWith (+)


mergeAll :: [IngredientList] -> IngredientList
mergeAll = foldr mergeIngredients M.empty


-- this function simply looks up how to make a specified quantity of one ingredient,
-- and doesn't follow the trail back any further. If any quantity can't be made from
-- other ingredients, they are simply left in.
lookUpIngredient :: Text -> Int -> Reactions -> IngredientList
lookUpIngredient ingredient quantity r = case M.lookup ingredient r of
    Nothing -> M.singleton ingredient quantity
    Just (n, m) -> case compare quantity n of
        LT -> M.singleton ingredient quantity
        EQ -> m
        GT -> let (multiple, rem) = quantity `divMod` n
              in mergeIngredients (M.map (* multiple) m)
                    $ if rem > 0 then M.singleton ingredient rem else M.empty


-- this function uses the above to go all the way back to ore, along with some leftovers that
-- can't be made exactly with other ingredients
recursiveLookup :: Text -> Int -> Reactions -> IngredientList
recursiveLookup i q r
    | i == "ORE" = M.singleton "ORE" q
    | otherwise = fullyReduce needed
    where needed = lookUpIngredient i q r
          recurse i n = recursiveLookup i n r
          reduce = mergeAll . M.elems . M.mapWithKey recurse
          canReduce (i, q) = i /= "ORE" && let Just (n, _) = M.lookup i r in q >= n
          fullyReduce m
            | all (not . canReduce) (M.toList m) = m
            | otherwise = fullyReduce $ reduce m


-- helper function which might be useful - based on the idea that it's based to start by rounding up
-- those ingredients which need to go through the most conversions to get down to ore
stepsToOre :: Text -> Reactions -> Int
stepsToOre "ORE" _ = 1
stepsToOre i r = succ . maximum . map (flip stepsToOre r) . M.keys $
                    let Just (_, m) = M.lookup i r in m


-- this function is called when the recursive reduction above meets a dead end due to all the quantities
-- being less than the amount that can be made from a reaction. In this case we simply "round up" one
-- of the quantities to what it needs to be - I've chosen to take one of the ingredients which is
-- "furthest" from ore in terms of conversion steps , and it worked - but I'm really not sure why!
-- (Attempts to be more careful by considering all possibilities ran into the problem of exponential
-- explosion of possibilities for the more complex examples.)
roundUp :: Reactions -> IngredientList -> IngredientList
roundUp r m = let (furthest:rest) = reverse . sortOn steps $ M.toList m
              in M.fromList (roundupPair furthest : rest)
        where steps = flip stepsToOre r . fst
              roundupPair (i, n)
                | i == "ORE" = (i, n)
                | otherwise = let Just (m, _) = M.lookup i r in if n > 0 && n < m then (i, m) else (i, n)


oreToMake :: Text -> Int -> Reactions -> Int
oreToMake i q r = result $ fullReduction initialReduction
        where result m = let Just res = M.lookup "ORE" m in res
              fullReduction m
                | hasJustOre m = m
                | otherwise = fullReduction . reduce $ roundUp r m
              recurse i n = recursiveLookup i n r
              reduce = mergeAll . M.elems . M.mapWithKey recurse
              initialReduction = recursiveLookup i q r
              hasJustOre m = M.size (M.filter (> 0) m) == 1


solvePart1 :: Reactions -> Int
solvePart1 = oreToMake "FUEL" 1


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1


-- determines if the first ingredient list contains enough ingredients of all types to be able to make up
-- the second one. If not,this returns Nothing, otherwise it returns Just the quantities left.
ingredientsLeft :: IngredientList -> IngredientList -> Maybe IngredientList
ingredientsLeft have need = case sequence $ map snd allDiffs of
    Nothing -> Nothing
    _ -> Just . M.fromList $ map (second fromJust) allDiffs
    where ingredientsNeeded = M.keys need
          maybeDiff i = case M.lookup i have of
            Nothing -> (i, Nothing)
            Just q -> if (need ! i) < q
                then (i, Just $ q - (need ! i))
                else (i, Nothing)
          allDiffs = map maybeDiff ingredientsNeeded


-- takes a single ingredient plus a quantity, and goes down the reactions to see what can be made from it,
-- returning a list of all possibilities of what's left afterwards (including the result of the reaction)
canMakeFrom :: Text -> Int -> Reactions -> [IngredientList]
canMakeFrom ingredient quantity r = catMaybes $ map mergedResults reactionList
    where reactionList = M.toList r
          mergedResults (i, (n, ingList)) =
            let have = M.singleton ingredient quantity
                left = ingredientsLeft have ingList
                made = M.singleton i n
            in fmap (mergeIngredients made) left


-- this function takes a "list" of ingredients (actually implemented as a HashMap), and returns a list
-- of all outcomes from applying the above function to each ingredient
canMakeFromMulti :: IngredientList -> Reactions -> [IngredientList]
canMakeFromMulti m r = S.toList . S.fromList . concatMap expand $ M.keys m
    where expand i = let Just q = M.lookup i m
                         result = canMakeFrom i q r
                         rest = M.filterWithKey (\k _ -> k /= i) m
                     in map (mergeIngredients rest) result


-- same as above, but recursive, repeating until nothing in the list can be further processed
canMakeFromRecursive :: IngredientList -> Reactions -> [IngredientList]
canMakeFromRecursive m r
    | null reduction = [m]
    | otherwise = S.toList . S.fromList $ concatMap (flip canMakeFromRecursive r) reduction
    where reduction = canMakeFromMulti m r


totalFuelFromOre :: Int -> Reactions -> Int
totalFuelFromOre n = maximum . map readOre . canMakeFromRecursive (M.singleton "ORE" n)
    where readOre m = fromMaybe 0 (M.lookup "FUEL" m)


-- same problem as originally with part 1, exponential explosion of possibilities makes
-- this totally impractical. Need a shortcut, presumably with "steps" again
solvePart2 :: Reactions -> Int
solvePart2 = totalFuelFromOre 1000000000000


part2 :: IO Int
part2 = puzzleData >>= return . solvePart2
