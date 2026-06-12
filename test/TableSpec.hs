{-# LANGUAGE OverloadedStrings #-}

module TableSpec (tests) where

import Data.Time (Day, fromGregorian)
import Database.PostgresInterface.Sql.Plan
import Database.PostgresInterface.Table.Pushdown
import Test.Tasty
import Test.Tasty.HUnit

maxDay :: Day
maxDay = fromGregorian 9999 12 31

minDay :: Day
minDay = fromGregorian 1 1 1

tests :: TestTree
tests = testGroup "Table.Pushdown"
  [ testGroup "dateRange"
    [ testCase "returns Nothing when no predicates" testDateRangeEmpty
    , testCase "returns Nothing when predicate is on a different column" testDateRangeWrongCol
    , testCase "extracts lower bound from col > date" testDateRangeGt
    , testCase "extracts lower bound from col >= date" testDateRangeGe
    , testCase "extracts upper bound from col < date" testDateRangeLt
    , testCase "extracts upper bound from col <= date" testDateRangeLe
    , testCase "extracts range from AND of two predicates" testDateRangeAnd
    ]
  , testGroup "intRange"
    [ testCase "returns Nothing when no predicates" testIntRangeEmpty
    , testCase "extracts lower bound from col > n" testIntRangeGt
    , testCase "extracts upper bound from col < n" testIntRangeLt
    , testCase "extracts range from AND of two predicates" testIntRangeAnd
    ]
  ]

-- Helpers ----------------------------------------------------------------------

d :: Integer -> Int -> Int -> Day
d y m day = fromGregorian y m day

mkPlan :: [Predicate] -> QueryPlan
mkPlan preds = QueryPlan
  { qpSources    = [TableRef "t" Nothing]
  , qpColumns    = AllColumns
  , qpAliases    = []
  , qpPredicates = preds
  , qpOrderBy    = []
  , qpLimit      = Nothing
  , qpGroupBy    = []
  }

-- dateRange tests --------------------------------------------------------------

testDateRangeEmpty :: Assertion
testDateRangeEmpty =
  dateRange (mkPlan []) "event_date" @?= Nothing

testDateRangeWrongCol :: Assertion
testDateRangeWrongCol =
  dateRange (mkPlan [ColOp "amount" Gt (LitDate (d 2026 1 1))]) "event_date" @?= Nothing

testDateRangeGt :: Assertion
testDateRangeGt =
  dateRange (mkPlan [ColOp "event_date" Gt (LitDate (d 2026 1 1))]) "event_date"
    @?= Just (d 2026 1 2, maxDay)

testDateRangeGe :: Assertion
testDateRangeGe =
  dateRange (mkPlan [ColOp "event_date" Ge (LitDate (d 2026 1 1))]) "event_date"
    @?= Just (d 2026 1 1, maxDay)

testDateRangeLt :: Assertion
testDateRangeLt =
  dateRange (mkPlan [ColOp "event_date" Lt (LitDate (d 2026 6 30))]) "event_date"
    @?= Just (minDay, d 2026 6 29)

testDateRangeLe :: Assertion
testDateRangeLe =
  dateRange (mkPlan [ColOp "event_date" Le (LitDate (d 2026 6 30))]) "event_date"
    @?= Just (minDay, d 2026 6 30)

testDateRangeAnd :: Assertion
testDateRangeAnd =
  let lo = ColOp "event_date" Ge (LitDate (d 2026 1 1))
      hi = ColOp "event_date" Le (LitDate (d 2026 6 30))
  in dateRange (mkPlan [And lo hi]) "event_date"
       @?= Just (d 2026 1 1, d 2026 6 30)

-- intRange tests ---------------------------------------------------------------

testIntRangeEmpty :: Assertion
testIntRangeEmpty =
  intRange (mkPlan []) "amount" @?= Nothing

testIntRangeGt :: Assertion
testIntRangeGt =
  intRange (mkPlan [ColOp "amount" Gt (LitInt 100)]) "amount"
    @?= Just (101, maxBound)

testIntRangeLt :: Assertion
testIntRangeLt =
  intRange (mkPlan [ColOp "amount" Lt (LitInt 100)]) "amount"
    @?= Just (minBound, 99)

testIntRangeAnd :: Assertion
testIntRangeAnd =
  let lo = ColOp "amount" Ge (LitInt 50)
      hi = ColOp "amount" Le (LitInt 200)
  in intRange (mkPlan [And lo hi]) "amount"
       @?= Just (50, 200)
