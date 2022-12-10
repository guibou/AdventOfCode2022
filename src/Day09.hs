-- Start at 21:42
-- first at 21:59
-- first at 22:05
module Day09 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Set as Set
import Text.Megaparsec

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (concat <$> Prelude.many parseAction)

parseAction = do
  dir <- choice ["R" $> R,
          "U" $> U,
          "L" $> L,
          "D" $> D]
  _ <- " "
  c <- parseNumber
  _ <- "\n"
  pure (replicate c dir)

data Direction = R | U | L | D
  deriving (Show)

-- * Generics
moveHead (x, y) R = (x+1, y)
moveHead (x, y) L = (x-1, y)
moveHead (x, y) U = (x, y-1)
moveHead (x, y) D = (x, y+1)

moveHeadAndTail (head:tail) step = (head':tail')
 where
   head' = moveHead head step
   tail' = followHeads head' tail

followHeads _previous [] = []
followHeads previous (x:xs) = let
  x' = followHead previous x
  in x':followHeads x' xs

followHead (hx, hy) (tx, ty)
  -- We do not need to move
  | maxAxis == 1 = (tx, ty)
  | otherwise = (tx + max (-1) (min 1 dx), ty + max (-1) (min 1 dy))
  where
    maxAxis = max (abs dx) (abs dy)
    dx = hx - tx
    dy = hy - ty

-- * FIRST problem
move n steps = scanl moveHeadAndTail (replicate n (0 :: Int, 0)) steps

-- * SECOND problem
day :: _ -> Int
day = Set.size . Set.fromList . map (Unsafe.last) . move 2

day' = Set.size . Set.fromList . map (Unsafe.last) . move 10
-- * Tests

ex = parseContent [fmt|\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|]

ex' = parseContent [fmt|\
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 13
    it "of second star" $ do
      day' ex `shouldBe` 1
      day' ex' `shouldBe` 36
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 6339
    it "on second star" $ do
      day' fileContent `shouldBe` 2541
