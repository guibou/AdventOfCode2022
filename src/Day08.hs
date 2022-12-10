-- Start at 21;08
-- Star at 21:19
-- 2star at 21:30
module Day08 where

import Data.List qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = parse2D (unsafeRead @Int . Text.singleton)

-- * Generics

listVisibleOnLineOneDirection l = go (zip [0 ..] l) (-1)
  where
    go [] _ = []
    go ((pos, x) : xs) currentMaxHeight
      | x > currentMaxHeight = pos : go xs x
      | otherwise = go xs currentMaxHeight

listVisibleOnLineBothDirections l = Set.toList $ Set.fromList (vLeft <> vRight)
  where
    vLeft = listVisibleOnLineOneDirection l
    vRight = map (\x -> length l - 1 - x) $ listVisibleOnLineOneDirection (reverse l)

listVisibleGrid g = do
  (line, lineNo) <- zip g [0 ..]
  visCol <- listVisibleOnLineBothDirections line
  pure (lineNo, visCol)

listVisibleGridReverse g = map (\(x, y) -> (y, x)) (listVisibleGrid g')
  where
    g' = transpose g

listVisibleTree g = Set.fromList $ listVisibleGrid g <> listVisibleGridReverse g

-- * FIRST problem

day :: _ -> Int
day = Set.size . listVisibleTree

-- * SECOND problem

treeGrid g = Map.fromList $ do
  (l, line) <- zip [0 ..] g
  (c, tree) <- zip [0 ..] line
  pure ((l, c), tree)

walkGrid treeHeight (l, c) (dl, dc) g treeCount =
  case Map.lookup (l, c) g of
    Nothing -> treeCount
    Just treeHeight'
      | treeHeight' >= treeHeight -> treeCount + 1
      | otherwise -> walkGrid treeHeight (l + dl, c + dc) (dl, dc) g (treeCount + 1)

computeScenic treeHeight (l, c) g =
  let left = walkGrid treeHeight (l - 1, c) (-1, 0) g 0
      right = walkGrid treeHeight (l + 1, c) (1, 0) g 0
      up = walkGrid treeHeight (l, c + 1) (0, 1) g 0
      down = walkGrid treeHeight (l, c - 1) (0, -1) g 0
   in left * right * up * down

day' :: [[Int]] -> Int
day' g = Data.List.maximum $ do
  let grid = treeGrid g
  (lNo, line) <- zip [0 :: Int ..] g
  (cNo, curTreeHeight) <- zip [0 :: Int ..] line
  pure $ computeScenic curTreeHeight (lNo, cNo) grid

ex =
  parseContent
    [fmt|\
30373
25512
65332
33549
35390
|]

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 21
    it "of second star" $ do
      day' ex `shouldBe` 8
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1698
    it "on second star" $ do
      day' fileContent `shouldBe` 672280
