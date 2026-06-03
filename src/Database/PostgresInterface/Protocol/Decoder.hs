{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Protocol.Decoder
  ( decodeStartup
  , decodeFrontendMessage
  ) where

import Data.Binary.Get (Get, getByteString, getInt32be, getWord8, runGetOrFail)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgresInterface.Protocol.Messages

-- | Decode the startup message.
-- Format: int32 total-length + int32 protocol-version + key\0val\0... + \0
decodeStartup :: ByteString -> Either String FrontendMessage
decodeStartup bs =
  case runGetOrFail parseStartup (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, msg) -> Right msg

parseStartup :: Get FrontendMessage
parseStartup = do
  _len <- getInt32be  -- total length (includes itself)
  ver  <- getInt32be  -- protocol version
  params <- parseKeyValues
  pure (StartupMessage ver params)

-- | Parse null-terminated key=value pairs until a lone \0 terminator
parseKeyValues :: Get [(Text, Text)]
parseKeyValues = do
  key <- getCString
  if BS.null key
    then pure []  -- terminator
    else do
      val  <- getCString
      rest <- parseKeyValues
      pure ((decodeUtf8 key, decodeUtf8 val) : rest)

-- | Read bytes until \0
getCString :: Get ByteString
getCString = go []
  where
    go acc = do
      b <- getWord8
      if b == 0
        then pure (BS.pack (reverse acc))
        else go (b : acc)

-- | Decode a regular frontend message.
-- Format: type-byte (1) + int32 length (includes itself) + payload
decodeFrontendMessage :: ByteString -> Either String FrontendMessage
decodeFrontendMessage bs =
  case runGetOrFail parseFrontend (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, msg) -> Right msg

parseFrontend :: Get FrontendMessage
parseFrontend = do
  typeByte <- getWord8
  len      <- getInt32be  -- length includes itself
  let payloadLen = fromIntegral len - 4 :: Int
  payload  <- getByteString payloadLen
  case typeByte of
    0x51 -> parseQuery payload   -- 'Q'
    0x53 -> pure Sync            -- 'S'
    0x58 -> pure Terminate       -- 'X'
    _    -> fail ("unknown frontend message type: " <> show typeByte)

-- | Query payload: null-terminated SQL string
parseQuery :: ByteString -> Get FrontendMessage
parseQuery payload =
  -- strip the trailing null terminator
  pure $ Query $ decodeUtf8 $ BS.takeWhile (/= 0) payload
