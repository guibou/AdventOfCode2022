-- Start 22:52
-- star1 22:05
-- star2: 23:15
module Day10 where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Relude.Unsafe qualified as Unsafe
import Utils

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (Prelude.many (parseInstr <* "\n"))

-- * Generics

data Instr = Noop | AddX Int
  deriving (Show)

parseInstr = "noop" $> Noop <|> ("addx " *> (AddX <$> parseNumber))

runInstr x Noop = ([x], x)
runInstr x (AddX y) = ([x, x], x + y)

runInstrs _ [] = []
runInstrs x (i : is) =
  let (vals, x') = runInstr x i
   in vals ++ runInstrs x' is

-- * FIRST problem

day instrs =
  let res = runInstrs 1 instrs
   in sum $ map (\x -> x * (res Unsafe.!! (x - 1))) [20, 60 .. 220]

-- * SECOND problem

computLitPixels input = do
  (cycleNo, x) <- zip [1 ..] (runInstrs 1 input)
  let (line, col) = (cycleNo - 1) `divMod` 40
  px <- [x - 1, x, x + 1]
  guard $ px == col
  pure (line, col)

day' input =
  let litPixels = Set.fromList $ computLitPixels input
   in unlines $ do
        l <- [0 .. 5]
        pure $ Text.pack $ do
          c <- [0 .. 39]
          if (l, c) `Set.member` litPixels
            then pure '#'
            else pure ' '

-- * Tests

ex =
  parseContent
    [fmt|\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 13140
    it "of second star" $ do
      day' ex
        `shouldBe` [fmt|\
##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
###   ###   ###   ###   ###   ###   ### 
####    ####    ####    ####    ####    
#####     #####     #####     #####     
######      ######      ######      ####
#######       #######       #######     
|]
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 15680
    it "on second star" $ do
      day' fileContent
        `shouldBe` [fmt|\
#### #### ###  #### #  #  ##  #  # ###  
   # #    #  # #    #  # #  # #  # #  # 
  #  ###  ###  ###  #### #    #  # #  # 
 #   #    #  # #    #  # # ## #  # ###  
#    #    #  # #    #  # #  # #  # #    
#### #    ###  #    #  #  ###  ##  #    
|]
