module Day03 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Relude.Extra
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec

fileContent :: _
fileContent = parseContent $(getFile)

fileContent' :: _
fileContent' = parseContent' $(getFile)

parseContent :: Text -> _
parseContent s = map f (Text.lines s)
  where
    f t = Text.splitAt mid t
     where
       mid = Text.length t `div` 2

parseContent' :: Text -> _
parseContent' s = let
  lines = Text.lines s
  badges = (map computeBadge $ chunksOf 3 lines)
  in badges

computeBadge l = let
  [a, b, c] = map (\x -> Set.fromList $ Text.unpack $ x) l
  in Set.intersection a (Set.intersection b c)

-- * Generics


-- * FIRST problem
day rucksacks = sum $ map priority $ map f rucksacks
  where
    f (a, b) = Unsafe.head $ Set.toList $ Set.intersection (Set.fromList $ Text.unpack a) (Set.fromList $ Text.unpack b)

priority l 
  | l >= 'a' && l <= 'z' = ord l - ord 'a' + 1
  | otherwise = ord l - ord 'A' + 27

-- * SECOND problem
day' :: _ -> Int
day' content = sum (map (priority . Unsafe.head . Set.toList) content)

ex_in = [fmt|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
|]

ex = parseContent ex_in
ex' = parseContent' ex_in

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 157
    it "of second star" $ do
      day' ex' `shouldBe` 70
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 7831
    it "on second star" $ do
      day' fileContent' `shouldBe` 2683

--
-- Start: 23:03
-- End of both stars with live viewer yada: 23:30
