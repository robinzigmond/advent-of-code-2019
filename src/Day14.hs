{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Data.List (findIndices)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Lens.Micro.Platform (ix, (%~))


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
    Nothing -> M.fromList [(ingredient, quantity)]
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
    | i == "ORE" = M.fromList [("ORE", q)]
    | otherwise = fullyReduce needed
    where needed = lookUpIngredient i q r
          recurse i n = recursiveLookup i n r
          reduce = mergeAll . M.elems . M.mapWithKey recurse
          canReduce (i, q) = i /= "ORE" && let Just (n, _) = M.lookup i r in q >= n
          fullyReduce m
            | all (not . canReduce) (M.toList m) = m
            | otherwise = fullyReduce $ reduce m


-- this function is called when the recursive reduction above meets a dead end due to all the quantities
-- being less than the amount that can be made from a reaction. In this case we simply "round up" one
-- of the quantities to what it needs to be - and return a list of possible results, so we can later
-- test for which gives the minimum solution. (If all ingredients are just made from fuel, with no
-- intermediate steps, then there is clearly nothing better than to round all up at once, which we
-- do in this case.)
roundUp :: Reactions -> IngredientList -> [IngredientList]
roundUp r m
    = case findIndices (\(i,_) -> i /= "ORE" && any (/= "ORE") (allIngredients i)) $ M.toList m of
        [] -> [M.fromList . map roundupPair $ M.toList m]
        idxs -> map (\idx -> M.fromList . (ix idx %~ roundupPair) $ M.toList m) idxs
        where allIngredients i = let Just (_, is) = M.lookup i r in M.keys is
              roundupPair (i, n)
                | i == "ORE" = (i, n)
                | otherwise = let Just (m, _) = M.lookup i r in if n > 0 && n < m then (i, m) else (i, n)


oreToMake :: Text -> Int -> Reactions -> Int
oreToMake i q r = minimum . map result $ fullReduction initialReduction
        where result m = let Just res = M.lookup "ORE" m in res
              fullReduction m
                | hasJustOre m = [m]
                | otherwise = concatMap (fullReduction . reduce) $ roundUp r m
              recurse i n = recursiveLookup i n r
              reduce = mergeAll . M.elems . M.mapWithKey recurse
              initialReduction = recursiveLookup i q r
              hasJustOre m = M.size (M.filter (> 0) m) == 1


solvePart1 :: Reactions -> Int
solvePart1 = oreToMake "FUEL" 1


part1 :: IO Int
part1 = puzzleData >>= return . solvePart1
