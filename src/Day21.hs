-- start 12:18
-- first: 12:29
-- second: 13:10
module Day21 where

import Data.Map qualified as Map
import Text.Megaparsec
import Utils
import Prelude hiding (Op)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = Map.fromList . unsafeParse (Prelude.some (parseLine <* "\n"))

data Job = Number Int | BinOp JobName Op JobName
  deriving (Show)

newtype JobName = JobName String
  deriving (Ord, Eq, Show)

data Op = Add | Mul | Sub | Div
  deriving (Show)

parseJob =
  Number <$> parseNumber
    <|> ( BinOp <$> parseJobName <*> parseOp <*> parseJobName
        )

parseJobName :: ParsecT Void Text Identity JobName
parseJobName = JobName <$> Prelude.some (oneOf ['a' .. 'z'])

parseLine = do
  jobName <- parseJobName
  void ": "
  job <- parseJob
  pure (jobName, job)

parseOp =
  (" + " $> Add)
    <|> (" * " $> Mul)
    <|> (" - " $> Sub)
    <|> (" / " $> Div)

-- * Generics

solve name jobs =
  let evalJob job = case job of
        Number i -> i
        BinOp a op b ->
          let resA = result Map.! a
              resB = result Map.! b
           in evalOp op resA resB
      result = Map.fromList $ do
        (name, job) <- Map.toList jobs
        pure (name, evalJob job)
   in result Map.! name

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Mul = (*)
evalOp Sub = (-)
evalOp Div = div

-- * FIRST problem

day :: _ -> Int
day = solve root

root = JobName "root"

humn = JobName "humn"

-- * SECOND problem

day' jobs =
  let (BinOp sideA _ sideB) = jobs Map.! root

      depJob (Number _) = False
      depJob (BinOp a _ b) = a == humn || b == humn || depMap Map.! a || depMap Map.! b

      depMap = Map.fromList $ do
        (name, job) <- Map.toList jobs
        pure (name, name == humn || depJob job)

      solveEqual :: JobName -> Int -> Int
      solveEqual nameTarget equation
        | nameTarget == humn = equation
        | otherwise =
            let jobTarget = jobs Map.! nameTarget
             in case jobTarget of
                  Number _ -> error "Impossible, I need to find humn"
                  BinOp a op b -> case (depMap Map.! a, depMap Map.! b) of
                    (True, True) -> error "impossible, two sides"
                    (False, False) -> error $ "wtf impossible" <> show (a, b)
                    (True, False) ->
                      let bVal = solve b jobs
                       in case op of
                            Add -> solveEqual a (equation - bVal)
                            Sub -> solveEqual a (equation + bVal)
                            Mul -> solveEqual a (equation `div` bVal)
                            Div -> solveEqual a (equation * bVal)
                    (False, True) ->
                      let aVal = solve a jobs
                       in case op of
                            Add -> solveEqual b (equation - aVal)
                            Sub -> solveEqual b (-equation + aVal)
                            Mul -> solveEqual b (equation `div` aVal)
                            -- a / b == c ---> a == c * b --> b == a / c
                            Div -> solveEqual b (aVal `div` equation)
   in case (depMap Map.! sideA, depMap Map.! sideB) of
        (True, False) -> solveEqual sideA (solve sideB jobs)
        _ -> error "WTF"

-- * Tests

ex =
  parseContent
    [str|\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 152
    it "of second star" $ do
      day' ex `shouldBe` 301
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 223971851179174
    it "on second star" $ do
      day' fileContent `shouldBe` 3379022190351
