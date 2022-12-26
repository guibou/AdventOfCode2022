-- Start 22:52
-- End 223:13
module Day25 where

import Data.Text qualified as Text
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = Text.lines

-- * Generics

snafuFigitToInt = \case
  '2' -> 2
  '1' -> 1
  '0' -> 0
  '-' -> -1
  '=' -> -2

snafuToInt :: Text -> Int
snafuToInt l = go (Text.unpack l) 0
  where
    go [] acc = acc
    go (x : xs) acc = go xs (acc * 5 + snafuFigitToInt x)

intToSnafu :: Int -> Text
intToSnafu = Text.pack . reverse . go
  where
    go 0 = []
    go x =
      let (d, m) = x `divMod` 5
          (c, rest) = case m of
            0 -> ('0', 0)
            1 -> ('1', 0)
            2 -> ('2', 0)
            3 -> ('=', 1)
            4 -> ('-', 1)
       in c : go (d + rest)

-- * FIRST problem

day :: _ -> Text
day = intToSnafu . sum . map snafuToInt

-- * SECOND problem

day' :: _ -> Int
day' = undefined

-- * Tests

ex =
  parseContent
    [str|\
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` "2=-1=0"
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "2=20---01==222=0=0-2"
