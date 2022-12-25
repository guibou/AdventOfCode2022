-- Start at 16:36
-- First at 17:20
-- Second at 17:24
module Day23 where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Relude.Unsafe qualified
import Utils hiding (E)
import Prelude hiding (round)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Set (V2 Int)
parseContent t = Set.fromList $ do
  (lineNo, line) <- zip [0 ..] (Text.lines t)
  (colNo, c) <- zip [0 ..] (Text.unpack line)
  guard $ c == '#'
  pure (V2 colNo lineNo)

data Direction = N | NE | NW | S | SE | SW | W | E
  deriving (Show)

dirToDelta :: Direction -> V2 Int
dirToDelta = \case
  N -> V2 0 (-1)
  S -> V2 0 1
  W -> V2 (-1) 0
  E -> V2 1 0
  NE -> dirToDelta N + dirToDelta E
  NW -> dirToDelta N + dirToDelta W
  SE -> dirToDelta S + dirToDelta E
  SW -> dirToDelta S + dirToDelta W

checkNoElfIn elfs currentPos dirs = not (any (\dir -> (dirToDelta dir + currentPos) `Set.member` elfs) dirs)

-- * Generics

movingOptions =
  [ ([N, NE, NW], N),
    ([S, SE, SW], S),
    ([W, NW, SW], W),
    ([E, NE, SE], E)
  ]

round elfs (take 4 -> movingOptions) = do
  let proposals = Map.fromList $ do
        elf <- Set.toList elfs
        let noElfIn = checkNoElfIn elfs elf
        let findMove [] = []
            findMove ((a, b) : xs)
              | noElfIn a = pure b
              | otherwise = findMove xs

        move <-
          if
              | noElfIn [N, NE, NW, S, SE, SW, W, E] -> []
              | otherwise -> findMove movingOptions
        pure (elf, elf + dirToDelta move)
      conflicts = Map.keysSet $ Map.filter (> 1) $ Map.fromListWith (+) $ do
        (_, move) <- Map.toList proposals
        pure (move, 1 :: Int)

  Set.fromList $ do
    elf <- Set.toList elfs
    case Map.lookup elf proposals of
      Nothing -> pure elf
      Just pos'
        | pos' `Set.member` conflicts -> pure elf
        | otherwise -> pure pos'

rounds 0 _ elfs = elfs
rounds n movingOptions elfs = rounds (n - 1) (drop 1 movingOptions) (round elfs movingOptions)

-- * FIRST problem

solve n elfs = rounds n (cycle movingOptions) elfs

score (Set.toList -> elfs) =
  let mi = foldl' (liftA2 min) (Relude.Unsafe.head elfs) elfs
      ma = foldl' (liftA2 max) (Relude.Unsafe.head elfs) elfs
      V2 a b = ma - mi
   in (b + 1) * (a + 1) - length elfs

day :: _ -> Int
day = score . solve (10 :: Int)

-- * SECOND problem

day' :: _ -> Int
day' elfs = go 0 elfs (cycle movingOptions)
  where
    go !n elfs movingOptions =
      let elfs' = round elfs movingOptions
       in if elfs == elfs' then n + 1 else go (n + 1) elfs' (drop 1 movingOptions)

-- * Tests

exSmall =
  parseContent
    [str|\
.....
..##.
..#..
.....
..##.
.....
|]

ex =
  parseContent
    [str|\
....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
|]

draw elfs = display2DGrid $ Map.fromList $ do
  elf <- Set.toList elfs
  pure (elf, "#")

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 110
    it "of second star" $ do
      day' ex `shouldBe` 20
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 4218
    it "on second star" $ do
      day' fileContent `shouldBe` 976
