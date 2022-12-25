-- start 14:02
-- first at 15:03
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

data Facing = R | U | L | D deriving (Show, Enum, Bounded, Eq)

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

day' = score . solve wrapCube


wrapCube :: Map (V2 Int) b -> (V2 Int, Facing) -> (Facing, V2 Int)
wrapCube grid (pos@(V2 y x), facing) = nextPos
  where
    cubeSize = sqrtInt (Map.size grid `div` 6)
    (majorGridX, minorGridX) = x `divMod` cubeSize
    (majorGridY, minorGridY) = y `divMod` cubeSize

    toCube cubeId (miX, miY) = V2 (mgY * cubeSize + miY) (mgX * cubeSize + miX)
      where
        (mgX, mgY) = case cubeId :: Int of
                       1 -> (2, 0)
                       2 -> (0, 1)
                       3 -> (1, 1)
                       4 -> (2, 1)
                       5 -> (2, 2)
                       6 -> (3, 2)
                       _ -> error "Wrong cube Id" 

    nextPos = case traceShowId (majorGridX, majorGridY) of
      (2, 0) ->
        -- case 1
        case facing of
          U -> (D, toCube 2 (cubeSize - 1 - minorGridX, 0))
          L -> (D, toCube 3 (minorGridY, 0))
          R -> (L, toCube 6 (minorGridX, cubeSize - minorGridY - 1))
          _ -> error "Impossible"
      (0, 1) -> 
        -- case 2
        case facing of
                 U -> (D, toCube 1 (cubeSize - 1 - minorGridX, 0))
                 D -> (U, toCube 2 (cubeSize - 1 - minorGridX , cubeSize - 1))
                 L -> error "Not done yet"
                 _ -> error "Impossible"

      (1, 1) -> 
        -- case 3
        case facing of
                 U -> (R, toCube 1 (0, minorGridX))
                 D -> (R, toCube 5 (0, cubeSize - 1 - minorGridY))
                 _ -> error "Impossible"
      (2, 1) ->
        -- 4
        case facing of
          R -> (D, toCube 6 (cubeSize - minorGridY - 1, 0))
          _ -> error "Impossible"
      (2, 2) -> 
        -- 5
        case facing of
          L -> (U, toCube 3 (cubeSize - minorGridX - 1, cubeSize - 1))
          D -> (U, toCube 2 (cubeSize - 1 - minorGridX , cubeSize - 1))
          _ -> error "Impossible"
      (3, 2) -> 
        -- 6
        case facing of
                 U -> (L, toCube 4 (cubeSize - 1, cubeSize - 1 - minorGridX))
                 L -> error "Impossile"
                 R -> (L, toCube 1 (cubeSize - 1, cubeSize - 1 - minorGridY))
                 D -> error "Not done yet"

      _ -> error $ "Case of major grid impossible: " <> show (pos, (majorGridX, majorGridY))

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
      day' ex `shouldBe` 5031
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 123046
    it "on second star" $ do
      day' fileContent `shouldBe` 1238
