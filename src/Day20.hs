-- start at 15:23
-- Seriously... My code was written at 15:29... First star at 18:06. I made a mistake while "manually" copy pasting the result to the form...
-- second start: 18:08
module Day20 where

import Data.List (elemIndex)
import Data.Text qualified as Text
import Relude.Unsafe ((!!), fromJust)
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map unsafeRead . Text.lines

-- * Generics

ex =
  [ 1 :: Int,
    2,
    -3,
    3,
    -2,
    0,
    4
  ]

-- * FIRST problem

solve l' = go (l', []) 0
  where
    lMax = length l'
    go (a, b) currentPos
      | currentPos == lMax = a <> reverse b
    go ([], l) currentPos = go (reverse l, []) currentPos
    go (x@(xPos, xVal) : xs, rev) currentPos
      | xPos /= currentPos = go (xs, x : rev) currentPos
      | otherwise = go (moveBy (xVal `mod` (lMax - 1)) x (xs, rev)) (currentPos + 1)

decorate = zip [0 ..]

undecorate = map snd

moveBy :: Int -> a -> ([a], [a]) -> ([a], [a])
moveBy 0 v (l, lrev) = (v : l, lrev)
moveBy n v ([], lrev) = moveBy n v (reverse lrev, [])
moveBy n v (x : xs, lrev) = moveBy (n - 1) v (xs, x : lrev)

computeScore l' =
  let i0 = fromJust $ elemIndex 0 l'
      le = length l'
      c = map (\idx -> l' !! ((idx + i0) `mod` le)) [1000, 2000, 3000]
   in sum c

day :: _ -> _
day = computeScore . undecorate . solve . decorate

-- * SECOND problem

day' :: _ -> Int
day' = computeScore . undecorate . applyN 10 solve . decorate . map (* 811589153)

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 3
    it "of second star" $ do
      day' ex `shouldBe` 1623178306
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 6640
    it "on second star" $ do
      day' fileContent `shouldBe` 11893839037215
