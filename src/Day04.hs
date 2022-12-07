module Day04 where

import Utils
import qualified Data.Set as Set

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse parseElfs

-- * Generics

parseRange = (,) <$> parseNumber @Int <*> ("-" *> parseNumber)
parseTwoElfs = (,) <$> parseRange <*> ("," *> parseRange)

parseElfs = Prelude.some (parseTwoElfs <* "\n")

isFullyContained ((a, b), (c, d)) = 
  let
    s0 = Set.fromList [a..b]
    s1 = Set.fromList [c..d]
  in null (s0 `Set.difference` s1) ||
     null (s1 `Set.difference` s0)

isOvelapping ((a, b), (c, d)) = 
  let
    s0 = Set.fromList [a..b]
    s1 = Set.fromList [c..d]
  in not $ null (s0 `Set.intersection` s1)

-- * FIRST problem
day l = length $ filter isFullyContained l

-- * SECOND problem
day' l = length $ filter isOvelapping l

-- * Tests

ex = parseContent [fmt|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 2
    it "of second star" $ do
      day' ex `shouldBe` 4
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 528
    it "on second star" $ do
      day' fileContent `shouldBe` 881
--
--
-- starttime: 23:31
-- bothstar: 23:42
