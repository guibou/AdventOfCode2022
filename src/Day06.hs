module Day06 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Relude.Extra
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Data.List (nub)

fileContent :: _
fileContent = $(getFile)

-- * Generics


-- * FIRST problem
day content = let
  (s, xs) = splitAt 4 content
  go s (xs:xss) offset
    | length (nub s) == 4 = offset
    | otherwise = go (drop 1 s ++ [xs]) xss (offset + 1)
  in go s xs 4


day' content = let
  (s, xs) = splitAt 14 content
  go s (xs:xss) offset
    | length (nub s) == 14 = offset
    | otherwise = go (drop 1 s ++ [xs]) xss (offset + 1)
  in go s xs 14

-- * SECOND problem

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
