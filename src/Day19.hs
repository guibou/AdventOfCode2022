{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day19 where

import Control.Parallel.Strategies
import Data.Array
import Data.List (maximum)
import Data.Map qualified as Map
import Text.Megaparsec
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [BluePrint]
parseContent = unsafeParse (Prelude.some $ parseBluePrint <* "\n")

parseBluePrint :: Parser BluePrint
parseBluePrint = do
  _ <- "Blueprint "
  _ <- parseNumber @Int
  _ <- ":"
  robotCosts <- Text.Megaparsec.some parseRobotCost
  pure $ BluePrint robotCosts

parseRobotCost = do
  _ <- " Each "
  name <- parseName
  _ <- " robot costs "
  cost <- parseCost `sepBy` " and "
  _ <- "."
  pure (name, Map.fromList cost)

parseCost = do
  n <- parseNumber
  name <- parseName
  pure (name, n)

parseName :: Parser Robot
parseName =
  choice
    [ "ore" $> Ore,
      "clay" $> Clay,
      "obsidian" $> Obsidian,
      "geode" $> Geode
    ]

-- * Generics

data Robot = Ore | Clay | Obsidian | Geode
  deriving (Show, Ix, Ord, Eq)

newtype BluePrint = BluePrint [(Robot, Map Robot Int)]
  deriving (Show)

data Inventory = Inventory
  { income :: !(Map Robot Int),
    resources :: !(Map Robot Int),
    elapsed :: !Int
  }
  deriving (Show)

-- * FIRST problem

initInventory =
  Inventory
    { income = Map.singleton Ore 1,
      resources = mempty,
      elapsed = 0
    }

scoreBlueprint :: Int -> BluePrint -> Int
scoreBlueprint maxTime (BluePrint blueprint) =
  go
    [initInventory]
    0
  where
    go [] !maxGeodes = maxGeodes
    go (current : xs) maxGeodes =
      let (maxGeodes', newStates) = partitionEithers $ map (\(limit, bp) -> advance maxGeodes maxTime limit current bp) blueprint_with_limits
       in go (newStates ++ xs) (maximum (maxGeodes : maxGeodes'))

    blueprint_with_limits = map (\(robot, cost) -> (Map.lookup robot maxBotCount, (robot, cost))) blueprint
    maxBotCount = Map.delete Geode $ foldl' f mempty blueprint
    f m (_, c) = Map.unionWith max m c

advance :: Int -> Int -> Maybe Int -> Inventory -> (Robot, Map Robot Int) -> Either Int Inventory
advance maxGeodes maxTime maxBotCount inventory (robot, robotCost)
  | interestingToBuy maxBotCount (Map.lookup robot inventory.income),
    Just waitTime <- nbTurnToBuy tune inventory.income,
    inventory.elapsed + waitTime + 1 < maxTime =
      let remaining_time = maxTime - inventory.elapsed + waitTime + 1
          newInventory =
            inventory
              { elapsed = inventory.elapsed + waitTime + 1,
                resources =
                  Map.unionWith
                    (+)
                    tune
                    (fmap (* (waitTime + 1)) inventory.income),
                income = Map.unionWith (+) inventory.income (Map.singleton robot 1)
              }
       in if ((remaining_time - 1) * remaining_time) `div` 2
            + fromMaybe 0 (Map.lookup Geode newInventory.resources)
            + remaining_time * fromMaybe 0 (Map.lookup Geode newInventory.income)
            < maxGeodes
            then Left 0
            else Right newInventory
  | otherwise =
      -- We cannot buy this robot before the end
      -- Just return the current score
      Left (fromMaybe 0 (Map.lookup Geode inventory.resources) + (maxTime - inventory.elapsed) * fromMaybe 0 (Map.lookup Geode inventory.income))
  where
    tune = Map.unionWith (+) inventory.resources (fmap negate robotCost)

interestingToBuy :: Maybe Int -> Maybe Int -> Bool
interestingToBuy Nothing _ = True
interestingToBuy _ Nothing = True
interestingToBuy (Just max) (Just current) = current < max

nbTurnToBuy :: Map Robot Int -> Map Robot Int -> Maybe Int
nbTurnToBuy cost income = do
  case Map.toList (fmap negate (Map.filter (< 0) cost)) of
    [] -> Just 0
    costList ->
      maximum
        <$> traverse
          ( \(robot, cost) -> case Map.lookup robot income of
              Nothing -> Nothing
              Just incomeOf -> Just $ (cost + incomeOf - 1) `div` incomeOf
          )
          costList

day blueprint = sum $ zipWith (*) (parMap rseq (scoreBlueprint 24) blueprint) [1 ..]

-- * SECOND problem

solve' blueprints = parMap rseq (scoreBlueprint 32) (take 3 blueprints)

day' :: _ -> Int
day' = product . solve'

-- * Tests

ex =
  parseContent
    [str|\
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 33
    it "of second star" $ do
      solve' ex `shouldBe` [56, 62]
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1150
    it "on second star" $ do
      day' fileContent `shouldBe` 1238
