{-# LANGUAGE OverloadedStrings #-}

module Table.DynamicSpec (tests) where

import Conduit (yieldMany)
import Database.PostgresInterface.Protocol.Messages (BackendMessage (..))
import Database.PostgresInterface.Queryable (ColumnValue (..))
import Database.PostgresInterface.Sql.Plan
  ( ColumnSelection (..)
  , QueryPlan (..)
  )
import Database.PostgresInterface.Table (AnyTable, executeTable, executeTableRows, findTable)
import Database.PostgresInterface.Table.Dynamic
import Test.Tasty
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Test data

schemaDefs :: [ColumnDef]
schemaDefs =
  [ ColumnDef "name"  DynText    False
  , ColumnDef "score" DynInteger False
  ]

testRows :: [[ColumnValue]]
testRows =
  [ [PgTextVal "alice", PgInt4Val 90]
  , [PgTextVal "bob",   PgInt4Val 75]
  , [PgTextVal "carol", PgInt4Val 85]
  ]

emptyPlan :: QueryPlan
emptyPlan = QueryPlan
  { qpTable      = "scores"
  , qpColumns    = AllColumns
  , qpPredicates = []
  , qpOrderBy    = []
  , qpLimit      = Nothing
  }

-- ---------------------------------------------------------------------------
-- Tests

tests :: TestTree
tests = testGroup "Table.Dynamic"
  [ testCase "naiveDynamicTable is found by name"              testFindByName
  , testCase "naiveDynamicTable returns all rows"              testAllRows
  , testCase "executeTable includes RowDescription"            testHasRowDesc
  , testCase "executeTableRows has no RowDescription"          testNoRowDesc
  , testCase "executeTable returns CommandComplete"            testCommandComplete
  , testCase "SELECT count matches row count"                  testSelectCount
  , testCase "LIMIT is applied"                                testLimit
  , testCase "column projection via NamedColumns"              testProjection
  ]

dynTable :: [AnyTable]
dynTable = [ naiveDynamicTable schemaDefs "scores" (pure (yieldMany testRows)) ]

testFindByName :: Assertion
testFindByName =
  case findTable "scores" dynTable of
    Nothing -> assertFailure "table 'scores' not found"
    Just _  -> pure ()

testAllRows :: Assertion
testAllRows = do
  msgs <- executeTable (head dynTable) emptyPlan
  let dataRows = [ r | r@(DataRow _) <- msgs ]
  length dataRows @?= 3

testHasRowDesc :: Assertion
testHasRowDesc = do
  msgs <- executeTable (head dynTable) emptyPlan
  let hasRD = any isRowDesc msgs
  assertBool "RowDescription present" hasRD
  where
    isRowDesc (RowDescription _) = True
    isRowDesc _                  = False

testNoRowDesc :: Assertion
testNoRowDesc = do
  msgs <- executeTableRows (head dynTable) emptyPlan
  let hasRD = any isRowDesc msgs
  assertBool "RowDescription absent" (not hasRD)
  where
    isRowDesc (RowDescription _) = True
    isRowDesc _                  = False

testCommandComplete :: Assertion
testCommandComplete = do
  msgs <- executeTable (head dynTable) emptyPlan
  let hasCC = any isCC msgs
  assertBool "CommandComplete present" hasCC
  where
    isCC (CommandComplete _) = True
    isCC _                   = False

testSelectCount :: Assertion
testSelectCount = do
  msgs <- executeTableRows (head dynTable) emptyPlan
  let [CommandComplete tag] = [ m | m@(CommandComplete _) <- msgs ]
  tag @?= "SELECT 3"

testLimit :: Assertion
testLimit = do
  let plan = emptyPlan { qpLimit = Just 2 }
  msgs <- executeTableRows (head dynTable) plan
  let dataRows = [ r | r@(DataRow _) <- msgs ]
  length dataRows @?= 2

testProjection :: Assertion
testProjection = do
  let plan = emptyPlan { qpColumns = NamedColumns ["name"] }
  msgs <- executeTable (head dynTable) plan
  -- RowDescription should have 1 field
  let [RowDescription fields] = [ m | m@(RowDescription _) <- msgs ]
  length fields @?= 1
  -- Each DataRow should have 1 column
  let dataRows = [ cols | DataRow cols <- msgs ]
  mapM_ (\cols -> length cols @?= 1) dataRows
