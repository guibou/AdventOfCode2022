-- start at 11:49
-- first at 12:55
module Day17 (test) where

import Data.List (findIndex)
import Data.List qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Relude.Unsafe qualified
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Action]
parseContent = map toAction . Text.unpack . Text.strip

data Action = ToLeft | ToRight
  deriving (Show)

toAction '<' = ToLeft
toAction '>' = ToRight
toAction _ = error "WTF"

-- * Generics

newtype Rock = Rock (Set (V2 Int))
  deriving (Show)

toRock = Rock . Set.fromList

-- Rocks
rocksPatterns =
  Vector.fromList
    [ -- - shape
      toRock ((`V2` 0) <$> [0 :: Int .. 3]),
      -- + shape
      toRock [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2],
      -- L shape
      toRock [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2],
      -- \| Shape
      toRock $ V2 0 <$> [0 .. 3],
      -- Square shape
      toRock [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
    ]

chamberWidth = 7 :: Int

applyActionPos ToLeft = (V2 (-1) 0 +)
applyActionPos ToRight = (V2 1 0 +)

toWorldSet :: Rock -> V2 Int -> Set (V2 Int)
toWorldSet (Rock s) p = Set.map (p +) s

isInWorld (Rock s) =
  let min_x = Data.List.minimum (map (\(V2 x _) -> x) (Set.toList s))
      max_x = Data.List.maximum (map (\(V2 x _) -> x) (Set.toList s))
      min_y = Data.List.minimum (map (\(V2 _ y) -> y) (Set.toList s))
   in min_x >= 0 && max_x < chamberWidth && min_y >= 0

-- * FIRST problem

addBlockToWorld world rock pos = Set.union world (toWorldSet rock pos)

moveBlock world rock rockPos dPos
  | isInWorld (Rock $ toWorldSet rock newPos) && null (rockSet `Set.intersection` world) = Just newPos
  | otherwise = Nothing
  where
    newPos = dPos rockPos
    rockSet = toWorldSet rock newPos

solve nbBlocks initActions = startRockFall 0 mempty 0 0
  where
    actionsV = Vector.fromList initActions

    startRockFall falledBlocks world _ _
      | Just maxBlocks <- nbBlocks, maxBlocks == falledBlocks = [world]
      -- | isCompleteFloor world && falledBlocks > 0 = [Set.fromList [V2 0 falledBlocks]]
    startRockFall falledBlocks world rockId actionId = do
      let rock = rocksPatterns Vector.! rockId
      let (world', nextActionsId) = fallARock actionsV world rock actionId
      startRockFall (falledBlocks + 1) world' ((rockId + 1) `mod` Vector.length rocksPatterns) nextActionsId

fallARock actionsV world rock actionId = applyActionBlock startingRockPos actionId
  where
    applyActionBlock rockPos actionId =
      let a = actionsV Vector.! actionId
          as = (actionId + 1) `mod` Vector.length actionsV
       in case moveBlock world rock rockPos (applyActionPos a) of
            Just newPos -> applyFallAction newPos as
            Nothing -> applyFallAction rockPos as

    applyFallAction rockPos remainingActions = case moveBlock world rock rockPos (V2 0 (-1) +) of
      Just newPos -> applyActionBlock newPos remainingActions
      Nothing -> (addBlockToWorld world rock rockPos, remainingActions)

    max_y = case Set.toList world of
      [] -> -1
      l -> Data.List.maximum $ map (\(V2 _ y) -> y) l

    startingRockPos = V2 2 (max_y + 4)

display s = Text.unpack $ str2DGrid $ Map.fromList $ map ((,"#") . (\(V2 x y) -> V2 x (-y))) (Set.toList s) <> [(V2 0 1, "+"), (V2 7 1, "+")]

isCompleteFloor s = do
  let max_y = Data.List.maximum (map (\(V2 _ y) -> y) (Set.toList s))
  let at_y = Set.filter (\(V2 _ y) -> y == max_y) s
  Set.size at_y == 7

findFirstCompleteFloor n actions l = findIndex f (Set.empty : l)
  where
    lActions = length actions
    lCubes = length rocksPatterns
    f s = do
      let max_y = Data.List.maximum (map (\(V2 _ y) -> y) (Set.toList s))
      let at_y = Set.filter (\(V2 _ y) -> y == max_y) s
      Set.size at_y >= (7 - n)

day :: _ -> Int
day actions = Data.List.maximum ((\(V2 _ y) -> y) <$> Set.toList (Relude.Unsafe.last (solve (Just 2022) actions))) + 1

-- * SECOND problem

day' :: _ -> Int
day' = undefined

-- * Tests

ex =
  parseContent
    [str|\
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 3068
    it "of second star" $ do
      day' ex `shouldBe` 3055
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3055
    it "on second star" $ do
      day' fileContent `shouldBe` 1238
