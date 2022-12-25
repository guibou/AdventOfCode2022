-- start 17:59
module Day24 where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Path (shortestPath)
import Relude.Unsafe qualified
import Utils hiding (getBounds)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = do
  (lineNo, line) <- zip [1 :: Int ..] $ Relude.Unsafe.init $ drop 1 $ Text.lines t
  (colNo, c) <- zip [1 ..] $ Text.unpack (Text.init $ Text.drop 1 line)
  guard $ c /= '.'
  pure
    ( V2 colNo lineNo,
      case c of
        '^' -> U
        'v' -> D
        '>' -> R
        '<' -> L
        _ -> error "Unmatched bliz direction"
    )

-- * Generics

data Direction = U | D | R | L
  deriving (Show)

moveBlizzard :: _ -> (V2 Int, Direction) -> (V2 Int, Direction)
moveBlizzard (V2 maxX maxY) (pos, dir) = do
  let delta = case dir of
        U -> V2 0 (-1)
        D -> V2 0 1
        L -> V2 (-1) 0
        R -> V2 1 0
      pos'@(V2 x y) = pos + delta

      correctPos
        | x < 1 = V2 maxX y
        | y < 1 = V2 x maxY
        | x > maxX = V2 1 y
        | y > maxY = V2 x 1
        | otherwise = pos'
  (correctPos, dir)

moveBlizzards bounds = map (moveBlizzard bounds)

blizzards bounds = iterate (moveBlizzards bounds)

drawBlizzards :: [(V2 Int, Direction)] -> IO ()
drawBlizzards blizzards = do
  display2DGrid $ fmap dirToChar $ Map.fromListWith up $ ((\(pos, dir) -> (pos, One dir)) <$> blizzards)
  putStrLn "----"

dirToChar (One U) = "^"
dirToChar (One D) = "v"
dirToChar (One L) = "<"
dirToChar (One R) = ">"
dirToChar (Many i) = show i

data BlizCount = One Direction | Many Int

countBliz (One _) = 1
countBliz (Many x) = x

up a b = Many $ countBliz a + countBliz b

-- * FIRST problem

solveInner blizzardsSteps startPoint endPoint bounds@(V2 maxX maxY) = (w, path)
  where
    transition :: (V2 Int, Int) -> [(Int, (V2 Int, Int))]
    transition (currentPos, time) = do
      deltaPos <- connect4
      let pos'@(V2 x' y') = currentPos + deltaPos
      guard $ ((x' > 0 && x' <= maxX) && (y' > 0 && y' <= maxY)) || (pos' == startPoint) || (pos' == endPoint)
      guard $ pos' `Set.notMember` (blizzardsSteps Relude.Unsafe.!! (time + 1))
      pure (1, (pos', time + 1))

    Just (w, path) =
      shortestPath
        transition
        (+)
        (startPoint, 0)
        (\(v, _) -> v == endPoint)

solve blizs = solveInner blizzardsSteps startPoint endPoint bounds
  where
    -- Idx 0 represents blizzards at time 0
    blizzardsSteps = map (Set.fromList . map fst) $ blizzards bounds blizs
    bounds@(V2 maxX maxY) = getBounds (map fst blizs)

    startPoint = V2 1 0
    endPoint = V2 maxX (maxY + 1)

day = fst . solve

-- * SECOND problem

solve' blizs = w0 + w1 + w2
  where
    bounds@(V2 maxX maxY) = getBounds (map fst blizs)

    startPoint = V2 1 0
    endPoint = V2 maxX (maxY + 1)

    blizzardsSteps0 = map (Set.fromList . map fst) $ blizzards bounds blizs
    (w0, _) = solveInner blizzardsSteps0 startPoint endPoint bounds
    blizzardsSteps1 = drop w0 blizzardsSteps0
    (w1, _) = solveInner blizzardsSteps1 endPoint startPoint bounds
    blizzardsSteps2 = drop w1 blizzardsSteps1
    (w2, _) = solveInner blizzardsSteps2 startPoint endPoint bounds

day' = solve'

-- * Tests

simpleEx =
  parseContent
    [str|\
#.#####
#.....#
#<....#
#.....#
#...v.#
#.....#
#####.#
|]

ex =
  parseContent
    [str|\
#E######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 18
    it "of second star" $ do
      day' ex `shouldBe` 54
  describe "works" $ do
    it "on first star" $ do
      (day fileContent - 1) `shouldBe` 288
    it "on second star" $ do
      (day' fileContent - 1) `shouldBe` 861

getBounds = foldl' (liftA2 max) (V2 0 0)

-- 284 is too low
-- 289 is too hight
--
-- 862 is too high
-- 859 too low
