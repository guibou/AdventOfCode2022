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
import Control.Lens (ix, over)
import Data.Generics.Labels ()
import Data.Vector qualified as Vector
import Text.Megaparsec
import Utils hiding (over, set)

fileContent :: WorryNumber n => (Vector (MonkeyAttributes n), Vector (MonkeyStatus n))
fileContent = parseContent $(getFile)

parseContent :: WorryNumber n => Text -> (Vector (MonkeyAttributes n), Vector (MonkeyStatus n))
parseContent t =
  let (attrs, status) = unzip $ unsafeParse ((parseMonkey `sepBy` "\n\n") <* "\n") t
   in (Vector.fromList attrs, Vector.fromList status)

parseMonkey :: WorryNumber n => Parser (Monkey n)
parseMonkey = do
  void "Monkey "
  id <- parseNumber
  void ":"
  void "\n  Starting items: "
  startingItems <- reverse <$> (toWorryNumber <$> parseNumber) `sepBy` ", "
  void "\n  Operation: new = old "
  operator <- ("* " $> Mul) <|> ("+ " $> Add)
  operand <- Just <$> (toWorryNumber <$> parseNumber) <|> (Nothing <$ "old")
  let operation = (operator, operand)
  void "\n  Test: divisible by "
  test <- parseNumber
  void "\n    If true: throw to monkey "
  ifTrue <- parseNumber
  void "\n    If false: throw to monkey "
  ifFalse <- parseNumber

  let countOperations = 0

  pure $ assert (ifTrue /= id && ifFalse /= id) (MonkeyAttributes {..}, MonkeyStatus {..})

-- * Generics Modular Arithmetic

newtype Modular = Modular Int
  deriving (Show)

class WorryNumber n where
  add :: n -> n -> n
  mul :: n -> n -> n
  reduceWorryLevel :: n -> n
  isMod :: n -> Int -> Bool
  toWorryNumber :: Int -> n

instance WorryNumber Modular where
  add (Modular a) (Modular b) = Modular (a + b)

  mul (Modular a) (Modular b) = Modular (a * b)

  reduceWorryLevel (Modular m) = Modular (m`mod` (17* 3* 5* 7* 11* 19* 2* 13 * 23))

  toWorryNumber = Modular
  isMod (Modular idx) n = idx `mod` n == 0

instance WorryNumber Int where
  add = (+)
  mul = (*)
  reduceWorryLevel = (`div` 3)
  toWorryNumber = id
  isMod a b = a `mod` b == 0

-- * Monkey definition

data MonkeyAttributes n = MonkeyAttributes
  { id :: Int,
    operation :: (Operation, Maybe n),
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int
  }
  deriving (Show, Generic)

data MonkeyStatus n = MonkeyStatus
  { startingItems :: ![n],
    countOperations :: !Int
  }
  deriving (Show, Generic)

type Monkey n = (MonkeyAttributes n, MonkeyStatus n)

data Operation = Add | Mul
  deriving (Show, Generic)

-- * FIRST problem

round :: WorryNumber n => Vector (MonkeyAttributes n) -> Vector (MonkeyStatus n) -> Vector (MonkeyStatus n)
round attrs status = foldl' (flip $ monkeyRound attrs) status [0 .. Vector.length attrs - 1]

monkeyRound :: WorryNumber n => Vector (MonkeyAttributes n) -> Int -> Vector (MonkeyStatus n) -> Vector (MonkeyStatus n)
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

processItem :: WorryNumber n => MonkeyAttributes n -> Vector (MonkeyStatus n) -> n -> Vector (MonkeyStatus n)
processItem monkey status item = over (ix throwTo . #startingItems) (item' :) status
  where
    item' = reduceWorryLevel (applyOp monkey.operation item)
    throwTo =
      if item' `isMod` monkey.test
        then monkey.ifTrue
        else monkey.ifFalse

applyOp :: WorryNumber n => (Operation, Maybe n) -> n -> n
applyOp (op, val') x = case op of
  Add -> x `add` val
  Mul -> x `mul` val
  where
    val = fromMaybe x val'

nRounds :: WorryNumber n => Vector (MonkeyAttributes n) -> Int -> Vector (MonkeyStatus n) -> Vector (MonkeyStatus n)
nRounds _attrs 0 status = status
nRounds attrs n status = do
  let status' = Day11.round attrs status
  nRounds attrs (n - 1) status'

solve :: forall n. WorryNumber n => Int -> (Vector (MonkeyAttributes n), Vector (MonkeyStatus n)) -> Int
solve n (attrs, status) = product $ take 2 $ reverse $ sort $ Vector.toList $ fmap (.countOperations) $ nRounds attrs n status

day = solve @Int 20

day' = solve @Modular 10000

-- * SECOND problem

-- * Tests

ex :: WorryNumber n => (Vector (MonkeyAttributes n), Vector (MonkeyStatus n))
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
