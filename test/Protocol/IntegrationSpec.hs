{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.IntegrationSpec (tests, withTestServer) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (SomeException, bracket, try)
import Data.Binary.Put (putByteString, putInt32be, putWord8, runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Database.PostgresInterface.Serve (ServerConfig (..), servePostgres)
import Database.PostgresInterface.Table (AnyTable)
import Database.PostgreSQL.Simple (ConnectInfo (..), Only (..), defaultConnectInfo, query_)
import Database.PostgreSQL.Simple qualified as PG
import Network.Socket (Family (..), SockAddr (..), SocketType (..), bind, socket, socketPort)
import Network.Socket qualified as NS
import Network.Socket.ByteString (recv, sendAll)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Protocol.Integration"
  [ testCase "postgresql-simple connects and reaches ReadyForQuery" testConnect
  , testCase "connection closes cleanly on Terminate" testTerminate
  , testCase "SELECT version() returns a row" testVersion
  , testCase "SELECT current_schema() returns public" testCurrentSchema
  , testCase "SHOW transaction_isolation returns a value" testTxIsolation
  , testCase "Sync message is answered with ReadyForQuery" testSyncReadyForQuery
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
  bracket (PG.connect (testConnectInfo port)) PG.close action

testConnect :: Assertion
testConnect = withTestServer [] $ \port -> do
  result <- try (PG.connect (testConnectInfo port)) :: IO (Either SomeException PG.Connection)
  case result of
    Left err -> assertFailure ("Connection failed: " <> show err)
    Right conn -> PG.close conn

testTerminate :: Assertion
testTerminate = withTestServer [] $ \port -> do
  conn <- PG.connect (testConnectInfo port)
  PG.close conn

testVersion :: Assertion
testVersion = withTestServer [] $ \port -> do
  conn <- PG.connect (testConnectInfo port)
  rows <- query_ conn "SELECT version()" :: IO [Only String]
  PG.close conn
  case rows of
    [Only v] -> assertBool ("version should contain PostgreSQL, got: " <> v)
                           ("PostgreSQL" `startsWith` v || "PostgreSQL" `isInfixOf'` v)
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

testCurrentSchema :: Assertion
testCurrentSchema = withTestServer [] $ \port -> do
  conn <- PG.connect (testConnectInfo port)
  rows <- query_ conn "SELECT current_schema()" :: IO [Only String]
  PG.close conn
  case rows of
    [Only s] -> s @?= "public"
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

testTxIsolation :: Assertion
testTxIsolation = withTestServer [] $ \port -> do
  conn <- PG.connect (testConnectInfo port)
  rows <- query_ conn "SHOW transaction_isolation" :: IO [Only String]
  PG.close conn
  case rows of
    [Only _] -> pure ()  -- any value is fine
    _        -> assertFailure ("expected 1 row, got: " <> show (length rows))

-- | Send a Sync message (0x53) over a raw socket after startup handshake,
-- and assert the server responds with ReadyForQuery (0x5A), not ErrorResponse.
testSyncReadyForQuery :: Assertion
testSyncReadyForQuery = withTestServer [] $ \port -> do
  bracket (rawConnect port) NS.close $ \sock -> do
    -- Perform startup handshake
    sendAll sock startupMsg
    _handshake <- recvUntilReady sock
    -- Send Sync (0x53, length 4)
    sendAll sock syncMsg
    resp <- recv sock 1
    BS.index resp 0 @?= 0x5A  -- 'Z' = ReadyForQuery

-- | Build a minimal startup message for the raw socket test
startupMsg :: ByteString
startupMsg = BL.toStrict $ runPut $ do
  let body = BL.toStrict $ runPut $ do
        putInt32be 196608  -- protocol 3.0
        putByteString "user" >> putWord8 0
        putByteString "rawtest" >> putWord8 0
        putWord8 0
  putInt32be (fromIntegral (4 + BS.length body))
  putByteString body

-- | Sync message: type byte 0x53 + int32 length 4 (no payload)
syncMsg :: ByteString
syncMsg = BL.toStrict $ runPut $ do
  putWord8 0x53
  putInt32be 4

-- | Open a raw TCP socket to the test server
rawConnect :: Int -> IO NS.Socket
rawConnect port = do
  sock <- NS.socket NS.AF_INET NS.Stream 0
  NS.connect sock (NS.SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127,0,0,1)))
  pure sock

-- | Consume backend messages until we see a ReadyForQuery byte (0x5A)
recvUntilReady :: NS.Socket -> IO ()
recvUntilReady sock = do
  hdr <- recv sock 1
  if BS.null hdr
    then pure ()
    else do
      lenBytes <- recv sock 4
      let msgLen = readInt32BE lenBytes
      _payload <- recv sock (msgLen - 4)
      if BS.index hdr 0 == 0x5A
        then pure ()
        else recvUntilReady sock

readInt32BE :: ByteString -> Int
readInt32BE bs =
  fromIntegral (BS.index bs 0) * 16777216
  + fromIntegral (BS.index bs 1) * 65536
  + fromIntegral (BS.index bs 2) * 256
  + fromIntegral (BS.index bs 3)

-- Helpers
startsWith :: String -> String -> Bool
startsWith needle haystack = take (length needle) haystack == needle

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack =
  any (\i -> take (length needle) (drop i haystack) == needle)
      [0 .. length haystack - length needle]

