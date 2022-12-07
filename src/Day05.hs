{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Day05 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Vector as Vector

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = 
  let [a, b] = Text.splitOn "\n\n" t
  in (parseStacks a, unsafeParse (Prelude.many parseAction) b)

data Action = Action 
  {
    quantity :: Int,
    origin :: Int,
    destination :: Int
  }
  deriving (Show)

parseAction = do
  _ <- "move "
  quantity <- parseNumber
  _ <- "from "
  origin <- parseNumber
  _ <- "to "
  destination <- parseNumber
  _ <- "\n"
  pure $ Action{..}

-- * Generics


-- * FIRST problem
day (stacks, actions) = let
  finalStack = foldl' (flip applyAction) stacks actions
  in map (Unsafe.head) (Vector.toList finalStack)

-- * SECOND problem
day' (stacks, actions) = let
  finalStack = foldl' (flip applyAction') stacks actions
  in map (Unsafe.head) (Vector.toList finalStack)

-- * Tests
trim (' ':xs) = trim xs
trim o = o

parseStacks t = Vector.fromList $ map trim $ transpose $ map (map snd . filter (\(p, _c) -> p) . zip (cycle [True, False, False, False]) . drop 1 . Text.unpack) (Text.lines t)

ex = parseContent [fmt|\
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

applyAction :: Action -> Vector [Char] -> Vector [Char]
applyAction action input = let
  moveCrate:rest = input Vector.! (action.origin - 1)
  output = input Vector.// [
     (action.origin - 1, rest),
     (action.destination - 1, moveCrate: (input Vector.! (action.destination - 1)))
    ]
  in if action.quantity == 1 then output
     else applyAction (action { quantity = action.quantity - 1}) output

applyAction' :: Action -> Vector [Char] -> Vector [Char]
applyAction' action input = let
  (movedCrates, rest) = splitAt (action.quantity) (input Vector.! (action.origin - 1))
  output = input Vector.// [
     (action.origin - 1, rest),
     (action.destination - 1, movedCrates ++ (input Vector.! (action.destination - 1)))
    ]
  in output
  

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` "CMZ"
    it "of second star" $ do
      day' ex `shouldBe` "MCD"
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "GRTSWNJHH"
    it "on second star" $ do
      day' fileContent `shouldBe` "QLFQDBBHM"
--
-- start: 23:43
--
-- GRTRWNJTH is not the right answer
-- bath star at 00:22
