-- start 21:03
-- first star 21:48
-- second star 22:14
module Day14 where

import Data.List qualified
import Data.Text qualified as Text
import Text.Megaparsec
import Utils
import qualified Data.Map as Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = generateManyLines . unsafeParse (Prelude.some (parseTuples <* "\n"))

parseTuple = (,) <$> (parseNumber <*) "," <*> parseNumber

parseTuples = parseTuple `sepBy` "-> "

generateLine (x, y) (x', y')
  | x == x' = (x,) <$> [min y y' .. max y y']
  | y == y' = (,y) <$> [min x x' .. max x x']
  | otherwise = error "Impossible line"

generate1Lines l = concat $ zipWith generateLine l (drop 1 l)

generateManyLines l = Map.fromList $ (, Wall) <$> concatMap generate1Lines l

data Object = Wall | Sand
  deriving (Show, Eq)

-- * Generics

possibleMotion (x, y) usedCases
  | (x, y + 1) `Map.notMember` usedCases = Just (x, y + 1)
  | (x - 1, y + 1) `Map.notMember` usedCases = Just (x - 1, y + 1)
  | (x + 1, y + 1) `Map.notMember` usedCases = Just (x + 1, y + 1)
  | otherwise = Nothing

solve :: Map (Int, Int) Object -> Map (Int, Int) Object
solve level = propagate level (500, 0)
  where
    maxGrid = Data.List.maximum (map snd $ Map.keys level)
    propagate level curPos
      | snd curPos > maxGrid = level
      | otherwise = case possibleMotion curPos level of
          Nothing -> propagate (Map.insert curPos Sand level) (500, 0)
          Just pos' -> do
            propagate level pos'

possibleMotion' maxGrid (x, y) usedCases
  | y == (maxGrid - 1) = Nothing
  | (x, y + 1) `Map.notMember` usedCases = Just (x, y + 1)
  | (x - 1, y + 1) `Map.notMember` usedCases = Just (x - 1, y + 1)
  | (x + 1, y + 1) `Map.notMember` usedCases = Just (x + 1, y + 1)
  | otherwise = Nothing

solve' :: Map (Int, Int) Object -> Map (Int, Int) Object
solve' level = propagate level (500, 0)
  where
    maxGrid = Data.List.maximum (map snd $ Map.keys level) + 2
    propagate level curPos =
      case possibleMotion' maxGrid curPos level of
        Nothing ->
          let level' = Map.insert curPos Sand level
           in if curPos == (500, 0) then level' else propagate level' (500, 0)
        Just pos' -> do
          propagate level pos'

drawGrid :: Map (Int, Int) Object -> Text
drawGrid level = unlines $ do
  let allCases = Map.keys level

  (l :: Int) <- [0 .. (Data.List.maximum (map snd allCases))]
  pure $ Text.pack $ do
    c <- [(Data.List.minimum (map fst allCases)) .. (Data.List.maximum (map fst allCases))]
    pure $
      case Map.lookup (c, l) level of
        Nothing -> ' '
        Just Wall -> '#'
        Just Sand -> 'o'

-- * FIRST problem

day = Map.size . (Map.filter (==Sand)) . solve

-- * SECOND problem

day' = Map.size . (Map.filter (==Sand)) . solve'

-- * Tests

ex =
  parseContent
    [str|\
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 24
    it "of second star" $ do
      day' ex `shouldBe` 93
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 655
    it "on second star" $ do
      day' fileContent `shouldBe` 26484
