-- Start 18:41
-- first at 19:00
-- stoped for cooking
-- back at 19:11
-- end at 19:16
module Day12 where

import Data.List qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Path
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> (((Int, Int), (Int, Int)), Map (Int, Int) Int)
parseContent s = Map.fromList <$> foldl' f ((undefined, undefined), []) items
  where
    items = do
      (lineNo, l) <- zip [0 ..] (Text.lines s)
      (colNo, c) <- zip [0 ..] (Text.unpack l)

      pure ((lineNo, colNo), c)

f ((a, b), l) (pos, c)
  | c == 'S' = ((pos, b), (pos, ord 'a') : l)
  | c == 'E' = ((a, pos), (pos, ord 'z') : l)
  | otherwise = ((a, b), (pos, ord c) : l)

-- * Generics

transition m (x, y) = do
  let height = m Map.! (x, y)
  (dx, dy) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]
  let p = (x + dx, y + dy)
  case Map.lookup p m of
    Nothing -> []
    Just height'
      | height' - height <= 1 -> pure (1, p)
      | otherwise -> []

-- * FIRST problem

day :: _ -> Maybe Int
day ((start, end), m) = fst <$> shortestPath (transition m) (+) start end

-- * SECOND problem

day' :: _ -> Int
day' ((_, end), m) =
  let paths = shortestPathAll (transition (fmap negate m)) (+) end
   in Data.List.minimum $
        mapMaybe
          (\(start :: (Int, Int)) -> fst <$> paths start)
          (Map.keys $ Map.filter (== ord 'a') m)

-- * Tests

ex =
  parseContent
    [str|\
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` Just 31
    it "of second star" $ do
      day' ex `shouldBe` 29
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 425
    it "on second star" $ do
      day' fileContent `shouldBe` 418
