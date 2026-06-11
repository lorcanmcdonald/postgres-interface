{-# LANGUAGE OverloadedStrings #-}

-- | Tests documenting which SELECT query patterns are supported.
-- Each group is either "supported" (plan succeeds) or "not supported"
-- (plan fails with UnsupportedSyntax / parse error).
module Sql.SelectSupportSpec (tests) where

import Data.Text (Text)
import Database.PostgresInterface.Sql.Parse (parseSql)
import Database.PostgresInterface.Sql.Plan
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Sql.SelectSupport"
  [ testGroup "supported: basic SELECT"
    [ testCase "SELECT *"                   $ assertSupported "SELECT * FROM t"
    , testCase "SELECT columns"             $ assertSupported "SELECT a, b FROM t"
    , testCase "SELECT column alias"        $ assertSupported "SELECT date AS \"time\", amount FROM t"
    , testCase "SELECT with table alias"    $ assertSupported "SELECT * FROM t AS foo"
    ]

  , testGroup "supported: WHERE comparisons"
    [ testCase "text equality"              $ assertSupported "SELECT * FROM t WHERE cat = 'Food'"
    , testCase "numeric equality"           $ assertSupported "SELECT * FROM t WHERE n = 42"
    , testCase "greater than"              $ assertSupported "SELECT * FROM t WHERE n > 10"
    , testCase "less than"                 $ assertSupported "SELECT * FROM t WHERE n < 100"
    , testCase "greater than or equal"     $ assertSupported "SELECT * FROM t WHERE n >= 10"
    , testCase "less than or equal"        $ assertSupported "SELECT * FROM t WHERE n <= 100"
    , testCase "not equal (<>)"            $ assertSupported "SELECT * FROM t WHERE cat <> 'Food'"
    , testCase "date comparison"           $ assertSupported "SELECT * FROM t WHERE date >= '2024-01-01'"
    , testCase "ISO 8601 timestamp"        $ assertSupported "SELECT * FROM t WHERE date >= '2024-01-01T00:00:00Z'"
    , testCase "AND of two predicates"     $ assertSupported "SELECT * FROM t WHERE a = 'x' AND b = 'y'"
    , testCase "OR of two predicates"      $ assertSupported "SELECT * FROM t WHERE a = 'x' OR b = 'y'"
    , testCase "BETWEEN dates"             $ assertSupported "SELECT * FROM t WHERE date BETWEEN '2024-01-01' AND '2024-12-31'"
    , testCase "BETWEEN timestamps"        $ assertSupported "SELECT * FROM t WHERE date BETWEEN '2024-01-01T00:00:00Z' AND '2024-12-31T23:59:59Z'"
    , testCase "parenthesised predicate"   $ assertSupported "SELECT * FROM t WHERE (a = 'x')"
    ]

  , testGroup "supported: ORDER BY / LIMIT / GROUP BY"
    [ testCase "ORDER BY ASC"              $ assertSupported "SELECT * FROM t ORDER BY amount ASC"
    , testCase "ORDER BY DESC"             $ assertSupported "SELECT * FROM t ORDER BY date DESC"
    , testCase "ORDER BY default"          $ assertSupported "SELECT * FROM t ORDER BY date"
    , testCase "LIMIT"                     $ assertSupported "SELECT * FROM t LIMIT 100"
    , testCase "GROUP BY"                  $ assertSupported "SELECT * FROM t GROUP BY category"
    ]

  , testGroup "supported: column aliases"
    [ testCase "alias used as output name" $ do
        p <- runPlan "SELECT date AS \"time\", amount FROM t"
        qpAliases p @?= [("date", "time")]
    , testCase "no alias when none given" $ do
        p <- runPlan "SELECT date, amount FROM t"
        qpAliases p @?= []
    , testCase "column still looked up by original name" $ do
        p <- runPlan "SELECT date AS \"time\" FROM t"
        qpColumns p @?= NamedColumns ["date"]
    ]

  , testGroup "not supported: WHERE operators"
    [ testCase "LIKE"         $ assertUnsupported "SELECT * FROM t WHERE name LIKE '%foo%'"
    , testCase "ILIKE"        $ assertUnsupported "SELECT * FROM t WHERE name ILIKE '%foo%'"
    , testCase "IS NULL"      $ assertUnsupported "SELECT * FROM t WHERE col IS NULL"
    , testCase "IS NOT NULL"  $ assertUnsupported "SELECT * FROM t WHERE col IS NOT NULL"
    , testCase "IN list"      $ assertUnsupported "SELECT * FROM t WHERE cat IN ('a', 'b', 'c')"
    , testCase "NOT IN list"  $ assertUnsupported "SELECT * FROM t WHERE cat NOT IN ('a', 'b')"
    , testCase "NOT predicate"$ assertUnsupported "SELECT * FROM t WHERE NOT a = 'x'"
    ]

  , testGroup "not supported: FROM clause"
    [ testCase "JOIN"         $ assertUnsupported "SELECT * FROM t JOIN u ON t.id = u.id"
    , testCase "subquery"     $ assertUnsupported "SELECT * FROM (SELECT * FROM t) sub"
    , testCase "multiple tables" $ assertUnsupported "SELECT * FROM t, u"
    ]

  -- Aggregate functions and arithmetic expressions are parsed successfully
  -- (the planner does not validate column expressions), but execution silently
  -- omits them because the alias won't match any column in the schema.
  , testGroup "parsed but silently empty: SELECT expressions"
    -- Expressions with an alias succeed at the plan level; COUNT(*) without
    -- an alias fails because there is no column name to fall back on.
    [ testCase "aggregate SUM with alias" $ assertSupported  "SELECT SUM(amount) AS total FROM t"
    , testCase "COUNT(*) no alias"        $ assertUnsupported "SELECT COUNT(*) FROM t"
    , testCase "arithmetic with alias"    $ assertSupported  "SELECT amount * 2 AS doubled FROM t"
    ]

  -- DML statements parse successfully but are rejected with UnsupportedOperation
  -- (SQLSTATE 0A000) rather than a syntax error.
  , testGroup "not implemented: DML statements"
    [ testCase "INSERT"  $ assertNotImplemented "INSERT INTO t VALUES (1)"
    , testCase "UPDATE"  $ assertNotImplemented "UPDATE t SET x = 1 WHERE id = 1"
    , testCase "DELETE"  $ assertNotImplemented "DELETE FROM t WHERE id = 1"
    ]
  ]

-- Helpers ---------------------------------------------------------------------

runPlan :: Text -> IO QueryPlan
runPlan sql = case parseSql sql of
  Left err  -> assertFailure ("parse error: " <> err) >> undefined
  Right ast -> case toQueryPlan ast of
    Left err -> assertFailure ("plan error: " <> show err) >> undefined
    Right p  -> pure p

assertSupported :: Text -> Assertion
assertSupported sql = case parseSql sql of
  Left err  -> assertFailure ("parse error: " <> err)
  Right ast -> case toQueryPlan ast of
    Left err -> assertFailure ("expected supported, got: " <> show err)
    Right _  -> pure ()

assertUnsupported :: Text -> Assertion
assertUnsupported sql = case parseSql sql of
  Left _    -> pure ()   -- parse failure also counts as "not supported"
  Right ast -> case toQueryPlan ast of
    Left _  -> pure ()
    Right _ -> assertFailure "expected unsupported, but plan succeeded"

assertNotImplemented :: Text -> Assertion
assertNotImplemented sql = case parseSql sql of
  Left err  -> assertFailure ("expected parse to succeed, got: " <> err)
  Right ast -> case toQueryPlan ast of
    Left (UnsupportedOperation _) -> pure ()
    Left err  -> assertFailure ("expected UnsupportedOperation, got: " <> show err)
    Right _   -> assertFailure "expected plan to fail with UnsupportedOperation"
