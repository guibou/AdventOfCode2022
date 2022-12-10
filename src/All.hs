module All where

import Utils

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10

import Weigh
import Debug.Trace (traceMarkerIO)
import Test.Hspec (before_)

allDays = sortBy (comparing fst) $(thisModuleName)

-- Test all samples and return a timing for all
tests = hspec $ before_ (traceMarkerIO "Test") $ mapM_ (\(name, s) -> describe name s) $ allDays

-- Test all samples and return a timing for each
tests' = mapM_ (\(name, s) -> do
  traceMarkerIO ("Testing " <> name)
  hspec $ describe name s
  ) $ allDays

-- Test all samples and return a timing for each
testsWeigh = mainWith $ mapM_ (\(name, s) -> action name $ hspec $ describe name s) allDays
