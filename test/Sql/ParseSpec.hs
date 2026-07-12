{-# LANGUAGE OverloadedStrings #-}

module Sql.ParseSpec (tests) where

import Database.PostgresInterface.Sql.Parse (parseSql)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Sql.Parse"
  [ testCase "SELECT parses successfully"        testSelectParses
  , testCase "INSERT rejected at parse"          testInsertRejected
  , testCase "UPDATE rejected at parse"          testUpdateRejected
  , testCase "DELETE rejected at parse"          testDeleteRejected
  , testCase "invalid SQL rejected at parse"     testInvalidRejected
  ]

testSelectParses :: Assertion
testSelectParses = case parseSql "SELECT * FROM t" of
  Left err -> assertFailure ("expected parse to succeed, got: " <> show err)
  Right _  -> pure ()

testInsertRejected :: Assertion
testInsertRejected = case parseSql "INSERT INTO t VALUES (1)" of
  Left _  -> pure ()
  Right _ -> assertFailure "expected parseSql to reject INSERT"

testUpdateRejected :: Assertion
testUpdateRejected = case parseSql "UPDATE t SET x = 1 WHERE id = 1" of
  Left _  -> pure ()
  Right _ -> assertFailure "expected parseSql to reject UPDATE"

testDeleteRejected :: Assertion
testDeleteRejected = case parseSql "DELETE FROM t WHERE id = 1" of
  Left _  -> pure ()
  Right _ -> assertFailure "expected parseSql to reject DELETE"

testInvalidRejected :: Assertion
testInvalidRejected = case parseSql "not valid sql at all" of
  Left _  -> pure ()
  Right _ -> assertFailure "expected parseSql to reject invalid SQL"
