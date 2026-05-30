{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module EndToEndSpec (tests) where

import Conduit (yieldMany)
import Control.Exception (bracket, try)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Queryable.Generic ()
import Database.PostgresInterface.Table (anyTable, naiveTable)
import Database.PostgreSQL.Simple (Only (..), query_, query)
import Database.PostgreSQL.Simple qualified as PG
import Protocol.IntegrationSpec (withTestServer)
import Test.Tasty
import Test.Tasty.HUnit

-- | Test type with generic Queryable instance
data Person = Person { personName :: Text, personAge :: Int32 }
  deriving (Generic, Show)

instance Queryable Person

testPeople :: [Person]
testPeople = [Person "Alice" 30, Person "Bob" 25]

tests :: TestTree
tests = testGroup "EndToEnd"
  [ testCase "SELECT * returns all rows" testSelectAll
  , testCase "SELECT * LIMIT 1 returns one row" testSelectLimit
  , testCase "SELECT named columns" testSelectColumns
  , testCase "unknown table returns error" testUnknownTable
  , testCase "naiveTable: WHERE filters rows" testNaiveWhere
  , testCase "naiveTable: ORDER BY sorts rows" testNaiveOrderBy
  , testCase "naiveTable: LIMIT applies after sort" testNaiveLimit
  ]

withPeopleServer :: (Int -> IO a) -> IO a
withPeopleServer = withTestServer
  [ anyTable "person" (\_plan -> pure (yieldMany testPeople)) ]

testSelectAll :: Assertion
testSelectAll = withPeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT * FROM person" :: IO [(Text, Int32)]
    rows @?= [("Alice", 30), ("Bob", 25)]

testSelectLimit :: Assertion
testSelectLimit = withPeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT * FROM person LIMIT 1" :: IO [(Text, Int32)]
    rows @?= [("Alice", 30)]

testSelectColumns :: Assertion
testSelectColumns = withPeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT personName FROM person" :: IO [Only Text]
    rows @?= [Only "Alice", Only "Bob"]

testUnknownTable :: Assertion
testUnknownTable = withPeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    result <- try (query_ conn "SELECT * FROM nonexistent" :: IO [(Text, Int32)])
    case result of
      Left (_ :: PG.SqlError) -> pure ()
      Right _                 -> assertFailure "expected error for unknown table"

withNaivePeopleServer :: (Int -> IO a) -> IO a
withNaivePeopleServer = withTestServer
  [ naiveTable "person" (pure (yieldMany testPeople)) ]

testNaiveWhere :: Assertion
testNaiveWhere = withNaivePeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT * FROM person WHERE personAge > 25" :: IO [(Text, Int32)]
    rows @?= [("Alice", 30)]

testNaiveOrderBy :: Assertion
testNaiveOrderBy = withNaivePeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT * FROM person ORDER BY personAge DESC" :: IO [(Text, Int32)]
    rows @?= [("Alice", 30), ("Bob", 25)]

testNaiveLimit :: Assertion
testNaiveLimit = withNaivePeopleServer $ \port ->
  bracket (PG.connect (connInfo port)) PG.close $ \conn -> do
    rows <- query_ conn "SELECT * FROM person ORDER BY personAge DESC LIMIT 1" :: IO [(Text, Int32)]
    rows @?= [("Alice", 30)]

connInfo :: Int -> PG.ConnectInfo
connInfo port = PG.defaultConnectInfo
  { PG.connectHost     = "127.0.0.1"
  , PG.connectPort     = fromIntegral port
  , PG.connectUser     = "testuser"
  , PG.connectPassword = ""
  , PG.connectDatabase = "testdb"
  }
