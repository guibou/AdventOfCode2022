module Day01 where

import Utils
import qualified Data.Text as Text
import Data.List (maximum)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [[Int]]
parseContent contentT = let
  blocks = Text.splitOn "\n\n" (Text.stripEnd contentT)
  subBlocks = fmap (map unsafeRead  . Text.splitOn "\n") blocks
  in subBlocks

-- * Generics


-- * FIRST problem
day :: [[Int]] -> Int
day = maximum . map sum

-- * SECOND problem
day' :: [[Int]] -> Int
day' = sum . take 3 . reverse . sort . map sum 

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 66616
    it "on second star" $ do
      day' fileContent `shouldBe` 199172
