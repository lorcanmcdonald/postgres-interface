{-# LANGUAGE OverloadedStrings #-}

module Protocol.MessagesSpec (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Binary.Put (putByteString, putInt32be, putWord8, runPut)
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Database.PostgresInterface.Protocol.Decoder (decodeStartup, decodeFrontendMessage)
import Database.PostgresInterface.Protocol.Encoder (encodeBackendMessage)
import Database.PostgresInterface.Protocol.Messages
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Protocol.Messages"
  [ testGroup "Encoder"
    [ testCase "AuthenticationOk" testAuthOk
    , testCase "ReadyForQuery Idle" testReadyForQueryIdle
    , testCase "ParameterStatus" testParameterStatus
    , testCase "CommandComplete SELECT" testCommandComplete
    , testCase "EmptyQueryResponse" testEmptyQueryResponse
    , testCase "ErrorResponse" testErrorResponse
    , testCase "RowDescription single field" testRowDescription
    , testCase "DataRow single text value" testDataRowText
    , testCase "DataRow NULL value" testDataRowNull
    ]
  , testGroup "Decoder"
    [ testCase "Startup message" testDecodeStartup
    , testCase "Query message" testDecodeQuery
    , testCase "Terminate message" testDecodeTerminate
    , testCase "Startup unknown param preserved" testDecodeStartupParams
    ]
  ]

-- Encoder tests ---------------------------------------------------------------

testAuthOk :: Assertion
testAuthOk =
  encodeBackendMessage AuthenticationOk
    @?= BS.pack [0x52, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00]

testReadyForQueryIdle :: Assertion
testReadyForQueryIdle =
  encodeBackendMessage (ReadyForQuery Idle)
    @?= BS.pack [0x5A, 0x00, 0x00, 0x00, 0x05, 0x49]

testParameterStatus :: Assertion
testParameterStatus =
  let msg = encodeBackendMessage (ParameterStatus "server_version" "14.0")
      expectedLen = 4 + 15 + 5
  in do
    BS.length msg @?= 1 + expectedLen
    BS.index msg 0 @?= 0x53  -- 'S'

testCommandComplete :: Assertion
testCommandComplete =
  let msg = encodeBackendMessage (CommandComplete "SELECT 3")
  in do
    BS.index msg 0 @?= 0x43  -- 'C'
    BS.length msg @?= 1 + 4 + 9

testEmptyQueryResponse :: Assertion
testEmptyQueryResponse =
  encodeBackendMessage EmptyQueryResponse
    @?= BS.pack [0x49, 0x00, 0x00, 0x00, 0x04]

testErrorResponse :: Assertion
testErrorResponse =
  let err = PgError SeverityError "42P01" "table \"foo\" does not exist"
      msg = encodeBackendMessage (ErrorResponse err)
  in do
    BS.index msg 0 @?= 0x45  -- 'E'
    elem 0x43 (BS.unpack msg) @?= True  -- 'C' SQLSTATE field code

testRowDescription :: Assertion
testRowDescription =
  let field = FieldInfo { fieldName = "id", fieldTypeOid = 23, fieldFormat = 0 }
      msg = encodeBackendMessage (RowDescription [field])
  in do
    BS.index msg 0 @?= 0x54  -- 'T'
    BS.index msg 5 @?= 0x00
    BS.index msg 6 @?= 0x01

testDataRowText :: Assertion
testDataRowText =
  let msg = encodeBackendMessage (DataRow [Just (BS.pack [0x34, 0x32])])
  in do
    BS.index msg 0 @?= 0x44  -- 'D'
    BS.index msg 5 @?= 0x00
    BS.index msg 6 @?= 0x01
    -- value length = 2: bytes 7-10 = 0x00 0x00 0x00 0x02
    BS.index msg 10 @?= 0x02

testDataRowNull :: Assertion
testDataRowNull =
  let msg = encodeBackendMessage (DataRow [Nothing])
  in do
    BS.index msg 0 @?= 0x44  -- 'D'
    BS.index msg 7 @?= 0xFF
    BS.index msg 8 @?= 0xFF
    BS.index msg 9 @?= 0xFF
    BS.index msg 10 @?= 0xFF

-- Decoder tests ---------------------------------------------------------------

-- | Build a raw startup message as a client would send it:
-- int32 total-length + int32 protocol-version + key\0val\0... + \0
buildStartup :: [(ByteString, ByteString)] -> ByteString
buildStartup params = BL.toStrict $ runPut $ do
  let body = BL.toStrict $ runPut $ do
        putInt32be 196608  -- protocol 3.0
        mapM_ (\(k, v) -> putByteString k >> putWord8 0 >> putByteString v >> putWord8 0) params
        putWord8 0  -- terminator
  putInt32be (fromIntegral (4 + BS.length body))
  putByteString body

testDecodeStartup :: Assertion
testDecodeStartup = do
  let raw = buildStartup [("user", "testuser"), ("database", "testdb")]
  case decodeStartup raw of
    Left err -> assertFailure ("decode failed: " <> err)
    Right (StartupMessage ver params) -> do
      ver @?= 196608
      lookup "user" params @?= Just "testuser"
      lookup "database" params @?= Just "testdb"
    Right other -> assertFailure ("unexpected message: " <> show other)

testDecodeStartupParams :: Assertion
testDecodeStartupParams = do
  let raw = buildStartup [("user", "alice"), ("application_name", "psql"), ("database", "mydb")]
  case decodeStartup raw of
    Left err -> assertFailure ("decode failed: " <> err)
    Right (StartupMessage _ params) ->
      lookup "application_name" params @?= Just "psql"
    Right other -> assertFailure ("unexpected: " <> show other)

-- | Build a regular frontend message: type-byte + int32-length + payload
buildFrontendMsg :: Word8 -> ByteString -> ByteString
buildFrontendMsg typeByte payload = BL.toStrict $ runPut $ do
  putWord8 typeByte
  putInt32be (fromIntegral (4 + BS.length payload))
  putByteString payload

testDecodeQuery :: Assertion
testDecodeQuery = do
  let sql = encodeUtf8 "SELECT 1" <> BS.singleton 0
      raw = buildFrontendMsg 0x51 sql  -- 'Q'
  case decodeFrontendMessage raw of
    Left err -> assertFailure ("decode failed: " <> err)
    Right (Query q) -> q @?= "SELECT 1"
    Right other -> assertFailure ("unexpected: " <> show other)

testDecodeTerminate :: Assertion
testDecodeTerminate = do
  let raw = buildFrontendMsg 0x58 BS.empty  -- 'X'
  case decodeFrontendMessage raw of
    Left err -> assertFailure ("decode failed: " <> err)
    Right Terminate -> pure ()
    Right other -> assertFailure ("unexpected: " <> show other)
