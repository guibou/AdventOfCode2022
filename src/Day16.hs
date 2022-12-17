{-# LANGUAGE OverloadedRecordDot #-}
-- start: 21:33
-- first star: 00:18
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Day16 (test) where

import Data.List qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Path (shortestPath)
import Text.Megaparsec
import Utils
import System.IO.Unsafe (unsafePerformIO)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t =
  Map.fromList $
    map (\v -> (v.name, v)) $
      unsafeParse (Prelude.many (parseValve <* "\n")) t

parseValve = do
  void "Valve "
  name <- parseValveId
  void " has flow rate="
  flowRate <- parseNumber
  void "; tunnel"
  void $ optional "s"
  void " lead"
  void $ optional "s"
  void " to valve"
  void $ optional "s"
  void " "
  tunnels <- parseValveId `sepBy` ", "

  pure $ Valve {..}

parseValveId = (\a b -> ValveId [a, b]) <$> anySingle <*> anySingle

newtype ValveId = ValveId {unId :: String}
  deriving stock (Show, Ord, Eq)
  deriving newtype (Hashable, Memoizable)

data Valve = Valve
  { name :: ValveId,
    flowRate :: Int,
    tunnels :: [ValveId]
  }
  deriving (Show, Generic)

-- * Generics

reverseMap valves = Map.fromListWith (++) $ do
  valve <- Map.elems valves
  childValve <- valve.tunnels
  pure (childValve, [valve.name])

valveWithFlow valves = Map.keys $ Map.filter (\v -> v.flowRate > 0) valves

-- * FIRST problem

makeASmallerGraph :: Map ValveId Valve -> Map ValveId [(ValveId, Int)]
makeASmallerGraph valves = allPaths
  where
    valvesReverse = reverseMap valves

    transition vId = (1,) <$> (goDown <> goUp)
      where
        goDown = case Map.lookup vId valves of
          Nothing -> error "WTF"
          Just valve -> valve.tunnels
        goUp = case Map.lookup vId valvesReverse of
          Nothing -> []
          Just parents -> parents

    allPaths = Map.fromListWith (++) $ do
      (v0, vs) <- select $ (ValveId "AA" : valveWithFlow valves)
      (v1, _) <- select vs

      let Just (weight, _) = shortestPath transition (+) v0 v1
      pure (v0, [(v1, weight)])

graphIt valves = do
  putStrLn "graph {"
  for_ valves $ \valve -> do
    when (valve.flowRate > 0) $ do
      putStrLn [fmt|{unId valve.name} [color=red]|]
  for_ valves $ \valve -> do
    for_ valve.tunnels $ \valveIdB -> do
      putStrLn [fmt|{unId valve.name} -> {unId valveIdB}|]
  putStrLn "}"

graphIt' valves = do
  let smallGraph = makeASmallerGraph valves
  putStrLn "graph {"
  for_ (Map.toList smallGraph) $ \(valveId, l) -> do
    for_ l $ \(valveIdB, cost) -> do
      when (valveId < valveIdB) $ do
        putStrLn [fmt|{unId valveId} <-> {unId valveIdB} [label={cost}]|]
  putStrLn "}"

solve (makeASmallerGraph -> valves) = go (ValveId "AA") (Set.singleton (ValveId "AA")) 0
  where
    go currentValve visited cost = do
      let nextValves = filter (\(name, _) -> name `Set.notMember` visited) $ valves Map.! currentValve
      if null nextValves
        then pure []
        else do
          (nextValve, nextCost) <- nextValves
          let newCost = cost + nextCost + 1
          if newCost > 30
            then pure []
            else ((nextValve, newCost) :) <$> go nextValve (Set.insert currentValve visited) newCost

weightPath :: Int -> Map _ _ -> _ -> Int
weightPath maxMinute valves path = finalAccum + (maxMinute - finalMinute) * (pressurePerMinute finalOpened)
  where
    (finalAccum, finalOpened, finalMinute) = foldl' f (0, mempty, 0) path
    pressurePerMinute openedValves = sum (map (\vId -> (valves Map.! vId).flowRate) (Set.toList openedValves))
    f (accumPressure, openedValves, previousMinute) (valve, activationMinute) =
      let 
          minutes = activationMinute - previousMinute
          
       in (accumPressure + pressurePerMinute openedValves * minutes, Set.insert valve openedValves, activationMinute)

-- * SECOND problem

day :: _ -> Int
day valves = Data.List.maximum $ map (weightPath 30 valves) $ solve valves

solve' (makeASmallerGraph -> valves) = go (ValveId "AA", ValveId "AA") ([ValveId "AA"]) (0, 0)
  where
    go = memoFix3 go'
    go' recCall currentValves (Set.fromList -> visited) currentCosts =
      let motionMe = nextMotion valves visited currentValves currentCosts
          motionElephant = map swapFirst (nextMotion valves visited (swap currentValves) (swap currentCosts))
          motions = motionMe <> motionElephant
       in do
            if null motions
              then pure []
              else do
                ((nextValves, nextCosts), nextVisited) <- motions
                ((nextValves, nextCosts) :) <$> recCall nextValves (Set.toList nextVisited) nextCosts

swapFirst ((a, b), v) = ((swap a, swap b), v)

nextMotion valves visited (currentValve, otherValve) (cost, otherCost) = do
  let nextValves = filter (\(name, _) -> name `Set.notMember` visited) $ valves Map.! currentValve
  if null nextValves
    then []
    else do
      (nextValve, nextCost) <- nextValves
      let newCost = cost + nextCost + 1
      if newCost > 26
        then []
        else pure (((nextValve, otherValve), (newCost, otherCost)), Set.insert nextValve visited)

weightPath' valves path = weightPath 26 valves (map f path) + weightPath 26 valves (map f' path)

f ((a, b), (c, d)) = (a, c)
f' ((a, b), (c, d)) = (b, d)

cache = unsafePerformIO (newIORef 0)
{-# NOINLINE cache #-}

traceBigger :: Int -> Int
traceBigger n = unsafePerformIO $ do
  curN <- readIORef cache
  if n > curN
     then do
       putStrLn (show n)
       writeIORef cache n
       pure n
      else pure n

day' valves = Data.List.maximum $ map (traceBigger . weightPath' valves) $ solve' valves
-- * Tests

ex =
  parseContent
    [str|\
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|]

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 1651
    it "of second star" $ do
      day' ex `shouldBe` 1707
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1906
    it "on second star" $ do
      day' fileContent `shouldBe` 2548

-- 2142 is too low
-- 2266 too low
