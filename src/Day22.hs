-- start 14:02
-- first at 15:03
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Day22 where

import Data.List (maximum, minimum)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Relude.Extra
import Relude.Unsafe qualified
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent input = do
  let (m, Text.drop 2 -> !instructions) = Text.breakOn "\n\n" input
  let grid = Map.fromList $ do
        (l, lineNo) <- zip (Text.lines m) [0 ..]
        (c, colNo) <- zip (Text.unpack l) [0 ..]
        guard $ c /= ' '
        pure
          ( V2 lineNo colNo,
            case c of
              '#' -> Wall
              '.' -> Free
          )

  (grid, unsafeParse (Prelude.many parseInstruction <* "\n") instructions)

parseInstruction = (Forward <$> parseNumber) <|> (Rotate <$> (("L" $> CounterClockWise) <|> ("R" $> ClockWise)))

data Space = Wall | Free
  deriving (Show, Ord, Eq)

data Facing = R | U | L | D deriving (Show, Enum, Bounded, Eq, Ord)

data Rotation = ClockWise | CounterClockWise deriving (Show)

rotateClockWise R = D
rotateClockWise x = prev x

rotateCounterClockWise D = R
rotateCounterClockWise x = succ x

data Instruction = Forward Int | Rotate Rotation deriving (Show)

-- * Generics

-- * FIRST problem

solve wrap (grid, instructions) = concat $ scanl' (flip (walk wrap grid)) [initPos grid] instructions

initPos grid = (minimum (Map.keys grid), R)

walk :: _ -> _ -> Instruction -> [(V2 Int, Facing)] -> [(V2 Int, Facing)]
walk _ _grid (Rotate r) (Relude.Unsafe.last -> (pos, facing)) = case r of
  ClockWise -> [(pos, rotateClockWise facing)]
  CounterClockWise -> [(pos, rotateCounterClockWise facing)]
walk _ _grid (Forward 0) v = [Relude.Unsafe.last v]
walk wrap grid (Forward i) s = case goForward wrap grid (Relude.Unsafe.last s) of
  Nothing -> [Relude.Unsafe.last s]
  Just s' -> Relude.Unsafe.last s : walk wrap grid (Forward (i - 1)) [s']

goForward :: _ -> _ -> (V2 Int, Facing) -> Maybe (V2 Int, Facing)
goForward wrap grid (pos@(V2 posy posx), facing) = do
  let delta = case facing of
        R -> V2 0 1
        L -> V2 0 (-1)
        U -> V2 (-1) 0
        D -> V2 1 0
      pos' = pos + delta

  case Map.lookup pos' grid of
    Just Wall -> Nothing
    Just Free -> Just (pos', facing)
    Nothing ->
      let (facing', pos'') = wrap grid (pos, facing)
       in case grid Map.! pos'' of
            Wall -> Nothing
            Free -> Just (pos'', facing')

wrapTorus grid ((V2 posy posx), facing) =
  ( facing,
    fst $ case facing of
      U -> maximum (filter (\(V2 _ x, _) -> x == posx) $ Map.toList grid)
      D -> minimum (filter (\(V2 _ x, _) -> x == posx) $ Map.toList grid)
      R -> minimum (filter (\(V2 y _, _) -> y == posy) $ Map.toList grid)
      L -> maximum (filter (\(V2 y _, _) -> y == posy) $ Map.toList grid)
  )

drawPath wrap input =
  let path = solve wrap input
      m = (Map.fromList $ map (\(pos, facing) -> (pos, drawFacing facing)) path) <> (fmap drawSpace (fst input))
   in display2DGrid (Map.fromList $ map (\(V2 x y, c) -> (V2 y x, c)) $ Map.toList m)

drawSpace :: Space -> Text
drawSpace Wall = "#"
drawSpace Free = "."

drawFacing :: Facing -> Text
drawFacing U = "^"
drawFacing D = "v"
drawFacing R = ">"
drawFacing L = "<"

day :: (Map (V2 Int) Space, [Instruction]) -> Int
day = score . solve wrapTorus

score res =
  let (V2 row column, facing) = Relude.Unsafe.last $ res
   in 1000 * (row + 1) + 4 * (column + 1) + scoreFacing facing

-- * SECOND problem

scoreFacing R = 0
scoreFacing D = 1
scoreFacing L = 2
scoreFacing U = 3

day' mapping = score . solve (wrapCube mapping)

mapCubeCoord cubeSize (minorGridX, minorGridY) from to =
  case (from, to) of
    -- R
    (R, R) -> (0, minorGridY)
    (R, U) -> (minorGridY, border)
    (R, D) -> (border - minorGridY, 0)
    (R, L) -> (border, border - minorGridY)
    -- L
    (L, L) -> (border, minorGridY)
    (L, R) -> (0, border - minorGridY)
    (L, U) -> (border - minorGridY, border)
    (L, D) -> (minorGridY, 0)
    -- U
    (U, U) -> (minorGridX, border)
    (U, D) -> (border - minorGridX, 0)
    (U, R) -> (0, minorGridX)
    (U, L) -> (border, border - minorGridX)
    -- D
    (D, D) -> (minorGridX, 0)
    (D, U) -> (border - minorGridX, border)
    (D, L) -> (border, minorGridX)
    (D, R) -> (0, border - minorGridX)
  where
    border = cubeSize - 1

wrapCube :: Mapping -> Map (V2 Int) b -> (V2 Int, Facing) -> (Facing, V2 Int)
wrapCube Mapping {..} grid (pos@(V2 y x), facing) = nextPos
  where
    cubeSize = sqrtInt (Map.size grid `div` 6)
    (majorGridX, minorGridX) = x `divMod` cubeSize
    (majorGridY, minorGridY) = y `divMod` cubeSize
    (-->) = mapCubeCoord cubeSize (minorGridX, minorGridY)

    toCube cubeId (miX, miY) = V2 (mgY * cubeSize + miY) (mgX * cubeSize + miX)
      where
        (mgX, mgY) = toFaceCube cubeId

    nextPos =
      let (nextCube, nextFacing) = (mapping Map.! (toCubeFace (majorGridX, majorGridY))) Map.! facing
       in (nextFacing, toCube nextCube (facing --> nextFacing))

data Mapping = Mapping
  { toCubeFace :: (Int, Int) -> Int,
    toFaceCube :: Int -> (Int, Int),
    mapping :: Map Int (Map Facing (Int, Facing))
  }

-- This is the mapping for the example
--
--         1111
--         1111
--         1111
--         1111
-- 222233334444
-- 222233334444
-- 222233334444
-- 222233334444
--         55556666
--         55556666
--         55556666
--         55556666
--
mappingExample =
  Mapping
    { mapping =
        [ ( 1,
            [ (U, (2, D)),
              (L, (3, D)),
              (R, (6, D))
            ]
          ),
          ( 2,
            [ (U, (1, D)),
              (D, (2, U))
            ]
          ),
          ( 3,
            [ (U, (1, R)),
              (D, (5, R))
            ]
          ),
          (4, [(R, (6, D))]),
          (5, [(L, (3, U)), (D, (2, U))]),
          (6, [(U, (4, L)), (R, (1, L))])
        ],
      toCubeFace = \case
        (2, 0) -> 1
        (0, 1) -> 2
        (1, 1) -> 3
        (2, 1) -> 4
        (2, 2) -> 5
        (3, 2) -> 6,
      toFaceCube = \case
        1 -> (2, 0)
        2 -> (0, 1)
        3 -> (1, 1)
        4 -> (2, 1)
        5 -> (2, 2)
        6 -> (3, 2)
    }

-- This is the mapping of my puzzle input
--  12
--  3
-- 45
-- 6

mappingPuzzleInput =
  Mapping
    { mapping =
        [ ( 1,
            [ (U, (6, R)),
              (L, (4, R))
            ]
          ),
          ( 2,
            [(U, (6, U)), (R, (5, L)), (D, (3, L))]
          ),
          ( 3,
            [(L, (4, D)), (R, (2, U))]
          ),
          ( 4,
            [(L, (1, R)), (U, (3, R))]
          ),
          ( 5,
            [(R, (2, L)), (D, (6, L))]
          ),
          ( 6,
            [ (L, (1, D)),
              (R, (5, U)),
              (D, (2, D))
            ]
          )
        ],
      toCubeFace = \case
        (1, 0) -> 1
        (2, 0) -> 2
        (1, 1) -> 3
        (0, 2) -> 4
        (1, 2) -> 5
        (0, 3) -> 6,
      toFaceCube = \case
        1 -> (1, 0)
        2 -> (2, 0)
        3 -> (1, 1)
        4 -> (0, 2)
        5 -> (1, 2)
        6 -> (0, 3)
    }

-- * Tests

sqrtInt :: Int -> Int
sqrtInt i = truncate @Float (sqrt (fromIntegral i))

ex :: (Map (V2 Int) Space, [Instruction])
ex =
  parseContent
    [str|\
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 6032
    it "of second star" $ do
      day' mappingExample ex `shouldBe` 5031
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 123046
    it "on second star" $ do
      day' mappingPuzzleInput fileContent `shouldBe` 195032
