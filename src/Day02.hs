-- Stat 20:51
module Day02 where

import Utils

fileContent :: _
fileContent = parseContent $(getFile)

data Hand = Rock | Paper | Scissor
  deriving (Show, Eq)

parseGames = many parseGame

parseGame = ((,) <$> parseP1 <*> (" " *> parseP2)) <* "\n"

parseP1 :: Parser Hand
parseP1 = Rock <$ "A"
          <|> Paper <$ "B"
          <|> Scissor <$ "C"

parseP2 :: Parser Hand
parseP2 = Rock <$ "X"
          <|> Paper <$ "Y"
          <|> Scissor <$ "Z"

parseContent :: Text -> _
parseContent = unsafeParse parseGames


-- * Generics
outcome Rock = 1
outcome Paper = 2
outcome Scissor = 3

data GameResult = Draw | Win | Lost
  deriving (Eq)

scoreGame Paper Rock = Win
scoreGame Rock Scissor = Win
scoreGame Scissor Paper = Win
scoreGame a b
  | a == b = Draw
  | otherwise = Lost

getAScore (opponent, me) = (case scoreGame me opponent of
                             Win -> 6
                             Draw -> 3
                             Lost -> 0) + outcome me

getAScore' (opponent, expectedOutcome) = do
  realMe <- [Rock, Paper, Scissor]
  guard $ scoreGame realMe opponent == outComeFromHand expectedOutcome
  pure $ getAScore (opponent, realMe)

outComeFromHand Rock = Lost
outComeFromHand Paper = Draw
outComeFromHand Scissor = Win

-- * FIRST problem
day :: _ -> Int
day = sum . map getAScore

-- * SECOND problem
day' :: _ -> Int
day' = sum . concatMap getAScore'

-- * Tests

ex = parseContent [str|\
A Y
B X
C Z
|]

-- 13334 is too high
-- 11873 is ok, at 21:04
--                 21:08 for second star

test :: Spec
test = do
 describe "simple examples" $ do
   it "of first star" $ do
     day ex `shouldBe` 15
   it "of second star" $ do
     day' ex `shouldBe` 12
 describe "works" $ do
   it "on first star" $ do
     day fileContent `shouldBe` 11873
   it "on second star" $ do
     day' fileContent `shouldBe` 12014
