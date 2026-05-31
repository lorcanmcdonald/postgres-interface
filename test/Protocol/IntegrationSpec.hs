{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.IntegrationSpec (tests, withTestServer) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, bracket, try)
import Database.PostgresInterface.Serve (ServerConfig (..), servePostgres)
import Database.PostgresInterface.Table (AnyTable)
import Database.PostgreSQL.Simple (ConnectInfo (..), Only (..), connect, defaultConnectInfo, query_)
import Database.PostgreSQL.Simple qualified as PG
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, socket, socketPort)
import Network.Socket qualified as NS
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Protocol.Integration"
  [ testCase "postgresql-simple connects and reaches ReadyForQuery" testConnect
  , testCase "connection closes cleanly on Terminate" testTerminate
  , testCase "SELECT version() returns a row" testVersion
  , testCase "SELECT current_schema() returns public" testCurrentSchema
  , testCase "SHOW transaction_isolation returns a value" testTxIsolation
  ]

-- | Start a test server on a random port, run the action, then stop.
withTestServer :: [AnyTable] -> (Int -> IO a) -> IO a
withTestServer tables action = do
  port <- findFreePort
  let cfg = ServerConfig { serverPort = port, serverHost = Just "127.0.0.1" }
  withAsync (servePostgres cfg tables) $ \_serverThread -> do
    threadDelay 50000  -- 50ms: let the server start listening
    action port

-- | Bind to port 0 to get an OS-assigned free port, then release it.
findFreePort :: IO Int
findFreePort = bracket
  (do s <- socket AF_INET Stream 0
      bind s (SockAddrInet 0 0)
      p <- socketPort s
      NS.close s
      pure (fromIntegral p))
  (const (pure ()))
  pure

testConnectInfo :: Int -> ConnectInfo
testConnectInfo port = defaultConnectInfo
  { connectHost     = "127.0.0.1"
  , connectPort     = fromIntegral port
  , connectUser     = "testuser"
  , connectPassword = ""
  , connectDatabase = "testdb"
  }

withConn :: Int -> (PG.Connection -> IO a) -> IO a
withConn port action =
  bracket (connect (testConnectInfo port)) PG.close action

testConnect :: Assertion
testConnect = withTestServer [] $ \port -> do
  result <- try (connect (testConnectInfo port)) :: IO (Either SomeException PG.Connection)
  case result of
    Left err -> assertFailure ("Connection failed: " <> show err)
    Right conn -> PG.close conn

testTerminate :: Assertion
testTerminate = withTestServer [] $ \port -> do
  conn <- connect (testConnectInfo port)
  PG.close conn

testVersion :: Assertion
testVersion = withTestServer [] $ \port -> do
  conn <- connect (testConnectInfo port)
  rows <- query_ conn "SELECT version()" :: IO [Only String]
  PG.close conn
  case rows of
    [Only v] -> assertBool ("version should contain PostgreSQL, got: " <> v)
                           ("PostgreSQL" `startsWith` v || "PostgreSQL" `isInfixOf'` v)
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

testCurrentSchema :: Assertion
testCurrentSchema = withTestServer [] $ \port -> do
  conn <- connect (testConnectInfo port)
  rows <- query_ conn "SELECT current_schema()" :: IO [Only String]
  PG.close conn
  case rows of
    [Only s] -> s @?= "public"
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

testTxIsolation :: Assertion
testTxIsolation = withTestServer [] $ \port -> do
  conn <- connect (testConnectInfo port)
  rows <- query_ conn "SHOW transaction_isolation" :: IO [Only String]
  PG.close conn
  case rows of
    [Only _] -> pure ()  -- any value is fine
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

-- Helpers
startsWith :: String -> String -> Bool
startsWith needle haystack = take (length needle) haystack == needle

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack =
  any (\i -> take (length needle) (drop i haystack) == needle)
      [0 .. length haystack - length needle]

