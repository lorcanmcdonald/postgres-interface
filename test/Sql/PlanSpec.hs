{-# LANGUAGE OverloadedStrings #-}

module Sql.PlanSpec (tests) where

import Data.Text (Text)
import Database.PostgresInterface.Sql.Parse (parseSql)
import Database.PostgresInterface.Sql.Plan
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Sql.Plan"
  [ testGroup "parseSql"
    [ testCase "parses simple SELECT *"     testParseSelectStar
    , testCase "parses SELECT with columns" testParseSelectCols
    , testCase "rejects INSERT"             testRejectInsert
    , testCase "rejects UPDATE"             testRejectUpdate
    ]
  , testGroup "toQueryPlan"
    [ testCase "SELECT * FROM t"                      testSelectStar
    , testCase "SELECT col FROM t"                    testSelectCol
    , testCase "SELECT * FROM t WHERE col = 'val'"   testWhereEq
    , testCase "SELECT * FROM t WHERE n > 42"        testWhereGt
    , testCase "SELECT * FROM t ORDER BY col ASC"    testOrderByAsc
    , testCase "SELECT * FROM t ORDER BY col DESC"   testOrderByDesc
    , testCase "SELECT * FROM t LIMIT 10"            testLimit
    , testCase "SELECT * FROM t GROUP BY col"        testGroupBy
    , testCase "WHERE a AND b"                       testWhereAnd
    , testCase "table name extracted correctly"      testTableName
    ]
  ]

-- Helpers ---------------------------------------------------------------------

runPlan :: Text -> IO QueryPlan
runPlan sql = case parseSql sql of
  Left err  -> assertFailure ("parse error: " <> err) >> undefined
  Right ast -> case toQueryPlan ast of
    Left err -> assertFailure ("plan error: " <> show err) >> undefined
    Right p  -> pure p

-- parseSql tests --------------------------------------------------------------

testParseSelectStar :: Assertion
testParseSelectStar =
  case parseSql "SELECT * FROM line_items" of
    Left err -> assertFailure ("parse failed: " <> err)
    Right _  -> pure ()

testParseSelectCols :: Assertion
testParseSelectCols =
  case parseSql "SELECT category, amount FROM line_items" of
    Left err -> assertFailure ("parse failed: " <> err)
    Right _  -> pure ()

testRejectInsert :: Assertion
testRejectInsert =
  case parseSql "INSERT INTO t VALUES (1)" of
    Left _  -> pure ()
    Right _ -> assertFailure "expected parse to fail for INSERT"

testRejectUpdate :: Assertion
testRejectUpdate =
  case parseSql "UPDATE t SET x = 1" of
    Left _  -> pure ()
    Right _ -> assertFailure "expected parse to fail for UPDATE"

-- toQueryPlan tests -----------------------------------------------------------

testSelectStar :: Assertion
testSelectStar = do
  p <- runPlan "SELECT * FROM line_items"
  qpColumns p @?= AllColumns
  qpTable   p @?= "line_items"

testSelectCol :: Assertion
testSelectCol = do
  p <- runPlan "SELECT category, amount FROM line_items"
  qpColumns p @?= NamedColumns ["category", "amount"]

testWhereEq :: Assertion
testWhereEq = do
  p <- runPlan "SELECT * FROM t WHERE category = 'Groceries'"
  qpPredicates p @?= [ColOp "category" Eq (LitText "Groceries")]

testWhereGt :: Assertion
testWhereGt = do
  p <- runPlan "SELECT * FROM t WHERE amount > 100"
  qpPredicates p @?= [ColOp "amount" Gt (LitNum 100)]

testOrderByAsc :: Assertion
testOrderByAsc = do
  p <- runPlan "SELECT * FROM t ORDER BY amount ASC"
  qpOrderBy p @?= [SortSpec "amount" Asc]

testOrderByDesc :: Assertion
testOrderByDesc = do
  p <- runPlan "SELECT * FROM t ORDER BY amount DESC"
  qpOrderBy p @?= [SortSpec "amount" Desc]

testLimit :: Assertion
testLimit = do
  p <- runPlan "SELECT * FROM t LIMIT 10"
  qpLimit p @?= Just 10

testGroupBy :: Assertion
testGroupBy = do
  p <- runPlan "SELECT * FROM t GROUP BY category"
  qpGroupBy p @?= ["category"]

testWhereAnd :: Assertion
testWhereAnd = do
  p <- runPlan "SELECT * FROM t WHERE a = 'x' AND b = 'y'"
  qpPredicates p @?= [And (ColOp "a" Eq (LitText "x")) (ColOp "b" Eq (LitText "y"))]

testTableName :: Assertion
testTableName = do
  p <- runPlan "SELECT * FROM my_table"
  qpTable p @?= "my_table"
