module Day06 where

import Utils
import Data.List (nub)

fileContent :: _
fileContent = $(getFile)

-- * Generics
solve n content = let
  (s, xs) = splitAt n content
  go s (xs:xss) offset
    | length (nub s) == n = offset
    | otherwise = go (drop 1 s ++ [xs]) xss (offset + 1)
  in go s xs n

-- * FIRST problem
day = solve 4

-- * SECOND problem
day' = solve 14

-- * Tests
ex = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
ex' = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 7
    it "of second star" $ do
      day' ex' `shouldBe` 26
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1987
    it "on second star" $ do
      day' fileContent `shouldBe` 3059

--start at 00:27
--end at 00:33
