{-# LANGUAGE OverloadedRecordDot #-}

-- Start at 11:00
-- First start at 11:17 with a bruteforce algo
module Day15 (test) where

import Control.Lens
import Relude.Unsafe qualified
import Utils hiding ((^.))

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse $ Prelude.many (parseSensor <* "\n")

parseSensor = do
  void "Sensor at x="
  x <- parseNumber
  void ", y="
  y <- parseNumber
  void ": closest beacon is at x="
  xBeacon <- parseNumber
  void ", y="
  yBeacon <- parseNumber

  pure (Sensor {posSensor = V2 x y, posBeacon = V2 xBeacon yBeacon})

data Sensor = Sensor
  { posSensor :: V2 Int,
    posBeacon :: V2 Int
  }
  deriving (Show)

-- * Generics

dist a b = let V2 x y = abs (a - b) in x + y

solve :: Int -> [Sensor] -> _
solve yScan sensors = sum $ map scoreRange $ getRanges yScan sensors

getRanges yScan sensors = compactRangeBis $ mapMaybe (getSafeRange yScan) sensors

getSafeRange yScan s = range
  where
    dy = abs ((s.posSensor) ^. _y - yScan)
    distBS = dist s.posSensor s.posBeacon
    dx = distBS - dy
    range =
      if dx > 0
        then Just $ Range (s.posSensor ^. _x - dx) (s.posSensor ^. _x + dx)
        else Nothing

data Range = Range Int Int
  deriving (Eq, Ord, Show)

compactRangeBis :: [Range] -> [Range]
compactRangeBis ranges = go (sortOn (\(Range a _b) -> a) ranges) (-100_000_000)
  where
    go [] _lastOffset = []
    go (Range a b : xs) lastOffset
      -- If the range is fully new
      | lastOffset < a = Range a b : go xs b
      -- If the range is skipped
      | lastOffset > b = go xs lastOffset
      -- If the range is partially contained
      | otherwise = Range (lastOffset + 1) b : go xs b

scoreRange (Range a b) = b - a + 1

-- * FIRST problem

day x sensors = solve x sensors - 1

-- * SECOND problem

day' sensors = Relude.Unsafe.head $ do
  -- Thank to lazyness, we can omit the max bound for the search area
  y <- [0 ..]
  let ranges = concatenateRanges $ getRanges y sensors
  guard $ length ranges > 1
  let x = missingVal ranges
  pure (x * 4_000_000 + y)

day'' sensors = do
  s <- sensors
  let safeDist = dist s.posSensor s.posBeacon
  [
    s.posSensor + V2 0 (safeDist + 1),
    s.posSensor - V2 0 (safeDist + 1),
    s.posSensor - V2 (safeDist + 1) 0,
    s.posSensor + V2 (safeDist + 1) 0
   ]

missingVal (Range _a b : _) = b + 1

concatenateRanges :: [Range] -> [Range]
concatenateRanges ranges = go (sortOn (\(Range a _b) -> a) ranges)
  where
    go [x] = [x]
    go (r@(Range a b) : r'@(Range b' c) : xs)
      | b + 1 == b' = concatenateRanges $ Range a c : xs
      | otherwise = r : go (r' : xs)
    go [] = error "WTF"

-- * Tests

ex =
  parseContent
    [str|\
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 10 ex `shouldBe` 26
    it "on second star" $ do
      day' ex `shouldBe` 56000011
  describe "works" $ do
    it "on first star" $ do
      day 2_000_000 fileContent `shouldBe` 4985193
    it "on second star" $ do
      day' fileContent `shouldBe` 11583882601918

-- 1158390601918 too low
