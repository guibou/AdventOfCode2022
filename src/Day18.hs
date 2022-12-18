{-# LANGUAGE LexicalNegation #-}

-- start 17:24
-- first 17:32
-- second: I've stopped for the match, kids were doing too much noise. Done at 18:23.
module Day18 where

import Data.List (maximum, minimum)
import Data.Set qualified as Set
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = Set.fromList . unsafeParse parseCubes

-- * Generics

parseCube = V3 <$> pn <*> pn <*> parseNumber
  where
    pn = parseNumber <* ","

parseCubes = Prelude.many (parseCube <* "\n")

-- * FIRST problem

getEmptyAsideCubes cubes = do
  cube <- Set.toList cubes
  d <-
    [ V3 -1 0 0,
      V3 1 0 0,
      V3 0 1 0,
      V3 0 -1 0,
      V3 0 0 1,
      V3 0 0 -1
      ]
  let closeCube = cube + d
  guard $ closeCube `Set.notMember` cubes
  pure closeCube

-- * SECOND problem

day :: Set (V3 Int) -> Int
day cubes = length $ getEmptyAsideCubes cubes

inBounds (mi, ma) p = liftA2 min p mi == mi && liftA2 max p ma == ma

-- * Tests

day' cubes = do
  let emptyCubes = getEmptyAsideCubes cubes
  let (xs, ys, zs) = unzip3 (map (\(V3 x y z) -> (x, y, z)) emptyCubes)
  let bounds =
        ( V3 (minimum xs) (minimum ys) (minimum zs),
          V3 (maximum xs) (maximum ys) (maximum zs)
        )

  let go visited curPoint
        | curPoint `Set.member` visited = visited
        | otherwise =
            let copainCubes = do
                  d <-
                    [ V3 -1 0 0,
                      V3 1 0 0,
                      V3 0 1 0,
                      V3 0 -1 0,
                      V3 0 0 1,
                      V3 0 0 -1
                      ]
                  let nextPoint = curPoint + d
                  guard $ inBounds bounds nextPoint
                  guard $ nextPoint `Set.notMember` cubes
                  pure nextPoint
             in foldl' go (Set.insert curPoint visited) copainCubes
  let outsidesCubes = go mempty (fst bounds)
  length $ filter (`Set.member` outsidesCubes) emptyCubes

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

ex :: Set (V3 Int)
ex =
  parseContent
    [str|\
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 64
    it "of second star" $ do
      day' ex `shouldBe` 58
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 3610
    it "on second star" $ do
      day' fileContent `shouldBe` 2082
