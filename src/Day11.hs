{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- Start at 10:11
-- First at 10:55
-- Second at 11:17
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day11 (test) where

import Control.Exception (assert)
import Control.Lens (ix, over, _1)
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import Text.Megaparsec
import Utils hiding (over, set)

fileContent :: (Vector MonkeyAttributes, Vector MonkeyStatus)
fileContent = parseContent $(getFile)

parseContent :: Text -> (Vector MonkeyAttributes, Vector MonkeyStatus)
parseContent t =
  let (attrs, status) = unzip $ unsafeParse ((parseMonkey `sepBy` "\n\n") <* "\n") t
   in (Vector.fromList attrs, Vector.fromList status)

data Operator = Add | Mul

parseMonkey :: Parser Monkey
parseMonkey = do
  void "Monkey "
  id <- parseNumber
  void ":"
  void "\n  Starting items: "
  startingItems <- reverse <$> parseNumber `sepBy` ", "
  void "\n  Operation: new = old "
  operator <- ("* " $> Mul) <|> ("+ " $> Add)
  operand <- Just <$> parseNumber <|> (Nothing <$ "old")
  let operation = case operator of
        Mul -> \x -> x * fromMaybe x operand
        Add -> \x -> x + fromMaybe x operand
  void "\n  Test: divisible by "
  test <- parseNumber
  void "\n    If true: throw to monkey "
  ifTrue <- parseNumber
  void "\n    If false: throw to monkey "
  ifFalse <- parseNumber

  let countOperations = 0

  pure $ assert (ifTrue /= id && ifFalse /= id) (MonkeyAttributes {..}, MonkeyStatus {..})

-- * Generics

reduceWorryLevelLarge :: Int -> Int
reduceWorryLevelLarge m = m `mod` (17 * 3 * 5 * 7 * 11 * 19 * 2 * 13 * 23)

reduceWorryLevelDiv3 :: Int -> Int
reduceWorryLevelDiv3 = (`div` 3)

-- * Monkey definition

data MonkeyAttributes = MonkeyAttributes
  { id :: Int,
    operation :: Int -> Int,
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int
  }
  deriving (Generic)

data MonkeyStatus = MonkeyStatus
  { startingItems :: ![Int],
    countOperations :: !Int
  }
  deriving (Show, Generic)

type Monkey = (MonkeyAttributes, MonkeyStatus)

-- * FIRST problem

round :: Vector MonkeyAttributes -> Vector MonkeyStatus -> Vector MonkeyStatus
round attrs status = foldl' (flip $ monkeyRound attrs) status [0 .. Vector.length attrs - 1]

monkeyRound :: Vector MonkeyAttributes -> Int -> Vector MonkeyStatus -> Vector MonkeyStatus
monkeyRound attrs monkeyNo status = do
  foldl' (processItem currentMonkey) status' items
  where
    status' =
      over
        (ix monkeyNo)
        ( \currentMonkey ->
            currentMonkey
              { countOperations = currentMonkey.countOperations + length items,
                startingItems = []
              }
        )
        status
    currentMonkey = attrs Vector.! monkeyNo
    items = reverse $ (status Vector.! monkeyNo).startingItems

processItem :: MonkeyAttributes -> Vector MonkeyStatus -> Int -> Vector MonkeyStatus
processItem monkey status item = over (ix throwTo . #startingItems) (item' :) status
  where
    item' = monkey.operation item
    throwTo =
      if item' `mod` monkey.test == 0
        then monkey.ifTrue
        else monkey.ifFalse

nRounds :: Vector MonkeyAttributes -> Int -> Vector MonkeyStatus -> Vector MonkeyStatus
nRounds _attrs 0 status = status
nRounds attrs n status = do
  let status' = Day11.round attrs status
  nRounds attrs (n - 1) status'

solve :: Int -> (Vector MonkeyAttributes, Vector MonkeyStatus) -> Int
solve n (attrs, status) = product $ take 2 $ reverse $ sort $ Vector.toList $ fmap (.countOperations) $ nRounds attrs n status

day = solve 20 . over (_1 . traverse . #operation) (reduceWorryLevelDiv3 .)

day' = solve 10000 . over (_1 . traverse . #operation) (reduceWorryLevelLarge .)

-- * SECOND problem

-- * Tests

ex :: (Vector MonkeyAttributes, Vector MonkeyStatus)
ex =
  parseContent
    [fmt|\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 10605
    it "of second star" $ do
      day' ex `shouldBe` 2713310158
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 110888
    it "on second star" $ do
      day' fileContent `shouldBe` 25590400731
