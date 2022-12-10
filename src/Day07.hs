-- start at 20:10
-- first start at 20:18
-- second star at 20:23
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day07 where

import Data.Foldable qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = go [] . Text.lines
  where
    go _path [] = []
    go path (l : xl)
      | "$" `Text.isPrefixOf` l = parseCommand path l xl
      | otherwise = do
          let (size, name) = Text.break (' ' ==) l
           in if size == "dir"
                then go path xl
                else (name : path, unsafeRead size) : go path xl

    parseCommand _path "$ cd /" xl = go [] xl
    parseCommand path "$ ls" xl = go path xl
    parseCommand path "$ cd .." xl = go (drop 1 path) xl
    parseCommand path cmd xl
      | "$ cd " `Text.isPrefixOf` cmd = go (Text.drop 5 cmd : path) xl
      | otherwise = error ("Unknown command" <> cmd)

computeDirSizes :: [([Text], Int)] -> Map [Text] Int
computeDirSizes l = Map.fromListWith (+) $ do
  (_fileName : path, size) <- l
  p <- inits (reverse path)

  pure (p, size)

-- * FIRST problem

day :: _ -> Int
day = sum . filter (< 100_000) . map snd . Map.toList . computeDirSizes

-- * SECOND problem

day' files =
  let dirSize = computeDirSizes files
      rootSize = dirSize Map.! []
      freeSpace = 70000000 - rootSize
      needToBeCleaned = 30000000 - freeSpace
   in Data.Foldable.minimum (filter (>= needToBeCleaned) $ map snd $ Map.toList dirSize)

-- * Tests

ex =
  parseContent
    [fmt|\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 95437
  it "of second star" $ do
    day' ex `shouldBe` 24933642
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1206825
    it "on second star" $ do
      day' fileContent `shouldBe` 9608311
