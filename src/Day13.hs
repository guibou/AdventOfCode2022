-- Start at 19:45
-- Start at 19:57
{-# LANGUAGE OverloadedLists #-}
-- Start at 19:45
-- Start at 19:57
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day13 where

import Data.List (elemIndex)
import GHC.Exts (IsList (..))
import Text.Megaparsec
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (parseTwoPackets `sepBy` "\n")

parseTwoPackets = (,) <$> pn <*> pn
  where
    pn = parsePacket <* "\n"

parsePacket =
  (PacketInt <$> parseNumber)
    <|> ("[" *> (Packet <$> sepBy parsePacket ",") <* "]")

-- * Generics

data Packet = Packet [Packet] | PacketInt Int
  deriving (Show, Eq)

-- So I can write nice `[1,[2,3]]` lists ;)
instance Num Packet where
  fromInteger i = PacketInt (fromInteger i)

instance IsList Packet where
  type Item Packet = Packet
  fromList = Packet

instance Ord Packet where
  compare (PacketInt a) (PacketInt b) = compare a b
  compare (Packet l) (Packet l') = compare l l'
  compare (Packet l) (PacketInt i) = compare (Packet l) (Packet [PacketInt i])
  compare (PacketInt i) (Packet l) = compare (Packet [PacketInt i]) (Packet l)

rightOrder (a, b) = case compare a b of
  GT -> False
  _ -> True

-- * FIRST problem

day pairs = sum $ map fst $ filter (\(_, p) -> rightOrder p) $ zip [1 :: Int ..] pairs

-- * SECOND problem

day' pairs =
  let packets = divider 2 : divider 6 : concatMap (\(a, b) -> [a, b]) pairs
      sPackets = sort packets
      Just i2 = elemIndex (divider 2) sPackets
      Just i6 = elemIndex (divider 6) sPackets
   in (i2 + 1) * (i6 + 1)

divider i = [[i]]

-- * Tests

ex =
  parseContent
    [str|\
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 13
    it "of second star" $ do
      day' ex `shouldBe` 140
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 5198
    it "on second star" $ do
      day' fileContent `shouldBe` 22344
