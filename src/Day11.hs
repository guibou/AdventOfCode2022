{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- Start at 10:11
-- First at 10:55
-- Second at 11:17
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day11 where

import Control.Exception (assert)
import Control.Lens (ix, over, set)
import Data.Generics.Labels ()
import Data.List qualified
import Data.Map qualified as Map
import Text.Megaparsec
import Utils hiding (over, set)

fileContent :: FunnyNumber n => Map Int (Monkey n)
fileContent = parseContent $(getFile)

parseContent :: FunnyNumber n => Text -> Map Int (Monkey n)
parseContent = toMonkeyMap . unsafeParse ((parseMonkey `sepBy` "\n\n") <* "\n")

toMonkeyMap monkies = Map.fromList $ do
  monkey <- monkies
  pure (monkey.id, monkey)

parseMonkey :: FunnyNumber n => Parser (Monkey n)
parseMonkey = do
  void "Monkey "
  id <- parseNumber
  void ":"
  void "\n  Starting items: "
  startingItems <- reverse <$> (toModular <$> parseNumber) `sepBy` ", "
  void "\n  Operation: new = old "
  operator <- ("* " $> Mul) <|> ("+ " $> Add)
  operand <- Just <$> (toModular <$> parseNumber) <|> (Nothing <$ "old")
  let operation = (operator, operand)
  void "\n  Test: divisible by "
  test <- parseNumber
  void "\n    If true: throw to monkey "
  ifTrue <- parseNumber
  void "\n    If false: throw to monkey "
  ifFalse <- parseNumber

  let countOperations = 0

  pure $ assert (ifTrue /= id && ifFalse /= id) $ Monkey {..}

-- * Generics Modular Arithmetic

newtype Modular = Modular (Map Int Int)
  deriving (Show, Generic, NFData)

modularLift op (Modular m) (Modular m') = Modular $ Map.unionWithKey (\k x y -> (x `op` y) `mod` k) m m'

class FunnyNumber n where
  modularAdd :: n -> n -> n
  modularMul :: n -> n -> n
  div3 :: n -> n
  isMod :: n -> Int -> Bool
  toModular :: Int -> n

instance FunnyNumber Modular where
  modularAdd = modularLift (+)

  modularMul = modularLift (*)

  div3 = id

  toModular o = Modular $ Map.fromList $ do
    -- n <- [17, 3, 5, 7, 11, 19, 2, 13]
    -- n <- [23, 19, 13, 17]
    n <- [1, 17, 3, 5, 7, 11, 19, 2, 13] <> [23]
    let !val = o `mod` n
    pure (n, val)
  isMod (Modular idx) n = idx Map.! n == 0

instance FunnyNumber Int where
  modularAdd = (+)
  modularMul = (*)
  div3 = (`div` 3)
  toModular = id
  isMod a b = a `mod` b == 0

-- * Monkey definition

data Monkey n = Monkey
  { id :: Int,
    startingItems :: [n],
    countOperations :: !Int,
    operation :: (Operation, Maybe n),
    test :: Int,
    ifTrue :: Int,
    ifFalse :: Int
  }
  deriving (Show, Generic, NFData)

data Operation = Add | Mul
  deriving (Show, NFData, Generic)

-- * FIRST problem

round :: NFData n => FunnyNumber n => Map Int (Monkey n) -> Map Int (Monkey n)
round monkeys = foldl' monkeyRound monkeys [0 .. Data.List.maximum (Map.keys monkeys)]

monkeyRound :: (FunnyNumber n, NFData n) => Map Int (Monkey n) -> Int -> Map Int (Monkey n)
monkeyRound monkies monkeyNo = do
  force $ foldl' (processItem currentMonkey) currentMonkey' items
  where
    !currentMonkey' = over (ix monkeyNo . #countOperations) (+ length items) $ set (ix monkeyNo . #startingItems) [] monkies
    currentMonkey = monkies Map.! monkeyNo
    items = reverse $ currentMonkey.startingItems

processItem :: FunnyNumber n => Monkey n -> Map Int (Monkey n) -> n -> Map Int (Monkey n)
processItem monkey monkies item = over (ix throwTo . #startingItems) (item' :) monkies
  where
    item' = div3 (applyOp monkey.operation item)
    throwTo =
      if item' `isMod` monkey.test
        then monkey.ifTrue
        else monkey.ifFalse

applyOp :: FunnyNumber n => (Operation, Maybe n) -> n -> n
applyOp (op, val') x = case op of
  Add -> x `modularAdd` val
  Mul -> x `modularMul` val
  where
    val = fromMaybe x val'

nRounds :: NFData n => FunnyNumber n => Map Int (Monkey n) -> Int -> Map Int (Monkey n)
nRounds monkies 0 = monkies
nRounds monkies n = do
  let monkies' = Day11.round monkies
  nRounds monkies' (n - 1)

solve :: forall n. NFData n => FunnyNumber n => Int -> Map Int (Monkey n) -> Int
solve n monkies = product $ take 2 $ reverse $ sort $ Map.elems $ fmap (.countOperations) $ nRounds monkies n

day = solve @Int 20

day' = solve @Modular 10000

countOps :: Monkey n -> Int
countOps = length . (.startingItems)

-- * SECOND problem

-- * Tests

ex :: FunnyNumber n => Map Int (Monkey n)
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
