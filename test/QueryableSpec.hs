{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module QueryableSpec (tests) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)
import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Queryable.Generic ()
import Test.Tasty
import Test.Tasty.HUnit

-- | A simple hand-written instance for testing
data Person = Person { personName :: Text, personAge :: Int32 }

instance Queryable Person where
  schema = [("name", PgText), ("age", PgInt4)]
  toRow (Person n a) = [PgTextVal n, PgInt4Val a]

-- | A record type that derives Queryable via GHC.Generics
data Event = Event { eventLabel :: Text, eventAmount :: Int32, eventDay :: Day }
  deriving (Generic)

instance Queryable Event

tests :: TestTree
tests = testGroup "Queryable"
  [ testGroup "Hand-written instance"
    [ testCase "schema returns correct column names" testSchemaNames
    , testCase "schema returns correct column types" testSchemaTypes
    , testCase "toRow produces correct values" testToRow
    , testCase "schema and toRow lengths match" testLengthInvariant
    , testCase "PgNull round-trips" testNull
    ]
  , testGroup "Generic deriving"
    [ testCase "schema column names from field names" testGenericSchemaNames
    , testCase "schema column types from Haskell types" testGenericSchemaTypes
    , testCase "toRow values match fields" testGenericToRow
    , testCase "schema and toRow lengths match" testGenericLengthInvariant
    ]
  ]

testSchemaNames :: Assertion
testSchemaNames =
  map fst (schema @Person) @?= ["name", "age"]

testSchemaTypes :: Assertion
testSchemaTypes =
  map snd (schema @Person) @?= [PgText, PgInt4]

testToRow :: Assertion
testToRow =
  toRow (Person "Alice" 30) @?= [PgTextVal "Alice", PgInt4Val 30]

testLengthInvariant :: Assertion
testLengthInvariant =
  length (toRow (Person "Alice" 30)) @?= length (schema @Person)

testNull :: Assertion
testNull = do
  let vals = [PgTextVal "x", PgNull, PgInt4Val 1]
  length vals @?= 3

-- Generic deriving tests -------------------------------------------------------

testGenericSchemaNames :: Assertion
testGenericSchemaNames =
  map fst (schema @Event) @?= ["eventLabel", "eventAmount", "eventDay"]

testGenericSchemaTypes :: Assertion
testGenericSchemaTypes =
  map snd (schema @Event) @?= [PgText, PgInt4, PgDate]

testGenericToRow :: Assertion
testGenericToRow =
  let day = fromGregorian 2026 1 19
      row = toRow (Event "Salary" 100 day)
  in row @?= [PgTextVal "Salary", PgInt4Val 100, PgDateVal day]

testGenericLengthInvariant :: Assertion
testGenericLengthInvariant =
  let day = fromGregorian 2026 1 1
  in length (toRow (Event "x" 1 day)) @?= length (schema @Event)
