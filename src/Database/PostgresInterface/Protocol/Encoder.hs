{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Protocol.Encoder
  ( encodeBackendMessage
  ) where

import Data.Binary.Put (Put, putByteString, putInt16be, putInt32be, putWord8, runPut)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgresInterface.Protocol.Messages

-- | Encode a server→client message to bytes per the PostgreSQL v3 wire protocol.
-- Format: type-byte (1) + length (int32, includes itself) + payload
encodeBackendMessage :: BackendMessage -> ByteString
encodeBackendMessage msg = BL.toStrict $ runPut $ case msg of
  AuthenticationOk ->
    -- 'R' + length=8 + int32 0
    tagged 0x52 $ putInt32be 0

  ReadyForQuery status ->
    -- 'Z' + length=5 + status byte
    tagged 0x5A $ putWord8 (txStatusByte status)

  ParameterStatus key val ->
    -- 'S' + length + key\0 + val\0
    tagged 0x53 $ do
      putCString key
      putCString val

  RowDescription fields ->
    -- 'T' + length + int16 field-count + field-descriptors
    tagged 0x54 $ do
      putInt16be (fromIntegral (length fields))
      mapM_ putFieldInfo fields

  DataRow cols ->
    -- 'D' + length + int16 col-count + for each: int32 len + bytes (or -1 for NULL)
    tagged 0x44 $ do
      putInt16be (fromIntegral (length cols))
      mapM_ putColumnValue cols

  CommandComplete tag ->
    -- 'C' + length + tag\0
    tagged 0x43 $ putCString tag

  EmptyQueryResponse ->
    -- 'I' + length=4 (no payload)
    tagged 0x49 $ pure ()

  ErrorResponse err ->
    -- 'E' + length + fields + \0 terminator
    tagged 0x45 $ do
      putErrorField 0x53 (severityText (pgErrorSeverity err))  -- S: severity
      putErrorField 0x56 (severityText (pgErrorSeverity err))  -- V: severity (non-localised)
      putErrorField 0x43 (pgErrorCode err)                     -- C: SQLSTATE
      putErrorField 0x4D (pgErrorMessage err)                  -- M: message
      putWord8 0x00                                            -- terminator

-- | Write a message with type byte + length-prefixed payload.
-- Length field = 4 (itself) + payload length.
tagged :: Word8 -> Put -> Put
tagged typeByte payload = do
  let payloadBytes = BL.toStrict $ runPut payload
  putWord8 typeByte
  putInt32be (fromIntegral (4 + BS.length payloadBytes))
  putByteString payloadBytes

-- | Null-terminated UTF-8 string
putCString :: Text -> Put
putCString t = do
  putByteString (encodeUtf8 t)
  putWord8 0x00

-- | Error response field: field-type byte + null-terminated string
putErrorField :: Word8 -> Text -> Put
putErrorField code val = do
  putWord8 code
  putCString val

-- | RowDescription field descriptor:
-- name\0 + tableOid(int32) + colAttr(int16) + typeOid(int32)
-- + typeSize(int16) + typeMod(int32) + format(int16)
putFieldInfo :: FieldInfo -> Put
putFieldInfo fi = do
  putCString (fieldName fi)
  putInt32be 0             -- table OID (0 = not a table column)
  putInt16be 0             -- column attribute number
  putInt32be (fieldTypeOid fi)
  putInt16be (-1)          -- type size (-1 = variable)
  putInt32be (-1)          -- type modifier
  putInt16be (fieldFormat fi)

putColumnValue :: Maybe ByteString -> Put
putColumnValue Nothing    = putInt32be (-1)
putColumnValue (Just bs)  = do
  putInt32be (fromIntegral (BS.length bs))
  putByteString bs

txStatusByte :: TxStatus -> Word8
txStatusByte Idle               = 0x49  -- 'I'
txStatusByte InTransaction      = 0x54  -- 'T'
txStatusByte FailedTransaction  = 0x45  -- 'E'

severityText :: ErrorSeverity -> Text
severityText SeverityError = "ERROR"
severityText SeverityFatal = "FATAL"
severityText SeverityPanic = "PANIC"
