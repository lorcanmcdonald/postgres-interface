{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Protocol.Decoder
  ( decodeStartup
  , decodeFrontendMessage
  ) where

import Data.Binary.Get (Get, getByteString, getInt16be, getInt32be, getWord8, runGetOrFail)
import Control.Monad (replicateM_, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
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
    0x51 -> parseQuery payload     -- 'Q'  Simple Query
    0x53 -> pure Sync              -- 'S'  Sync
    0x58 -> pure Terminate         -- 'X'  Terminate
    0x50 -> parseParse payload     -- 'P'  Parse
    0x44 -> parseDescribeFE payload -- 'D'  Describe
    0x42 -> parseBind payload      -- 'B'  Bind
    0x45 -> parseExecute payload   -- 'E'  Execute
    0x43 -> parseCloseFE payload   -- 'C'  Close
    _    -> fail ("unknown frontend message type: " <> show typeByte)

-- | Query payload: null-terminated SQL string
parseQuery :: ByteString -> Get FrontendMessage
parseQuery payload =
  -- strip the trailing null terminator
  pure $ Query $ decodeUtf8 $ BS.takeWhile (/= 0) payload

-- Extended query protocol payload parsers ------------------------------------

-- | Parse payload: name\0 query\0 int16(nparams) [int32 OID × nparams]
parseParse :: ByteString -> Get FrontendMessage
parseParse payload =
  case runGetOrFail go (BL.fromStrict payload) of
    Left  (_, _, err) -> fail ("malformed Parse: " <> err)
    Right (_, _, msg) -> pure msg
  where
    go = do
      name   <- getCString
      query  <- getCString
      nparams <- getInt16be
      replicateM_ (fromIntegral nparams) getInt32be  -- skip parameter OID hints
      pure (ParseMsg (decodeUtf8 name) (decodeUtf8 query))

-- | Describe payload: kind-byte ('S' or 'P') + name\0
parseDescribeFE :: ByteString -> Get FrontendMessage
parseDescribeFE payload =
  case runGetOrFail go (BL.fromStrict payload) of
    Left  (_, _, err) -> fail ("malformed Describe: " <> err)
    Right (_, _, msg) -> pure msg
  where
    go = do
      kind <- getWord8
      name <- getCString
      pure (DescribeMsg (toEnum (fromIntegral kind)) (decodeUtf8 name))

-- | Bind payload: portal\0 stmt\0 nFormats [fmt…] nParams [len+val…] nResultFmts [fmt…]
-- We extract the portal and statement names; everything else is ignored (no parameterised queries).
parseBind :: ByteString -> Get FrontendMessage
parseBind payload =
  case runGetOrFail go (BL.fromStrict payload) of
    Left  (_, _, err) -> fail ("malformed Bind: " <> err)
    Right (_, _, msg) -> pure msg
  where
    go = do
      portal  <- getCString
      stmt    <- getCString
      nFmts   <- getInt16be
      replicateM_ (fromIntegral nFmts) getInt16be
      nParams <- getInt16be
      replicateM_ (fromIntegral nParams) $ do
        len <- getInt32be
        when (len >= 0) $ void $ getByteString (fromIntegral len)
      -- ignore result-format codes
      pure (BindMsg (decodeUtf8 portal) (decodeUtf8 stmt))

-- | Execute payload: portal\0 + int32 max-rows
parseExecute :: ByteString -> Get FrontendMessage
parseExecute payload =
  case runGetOrFail go (BL.fromStrict payload) of
    Left  (_, _, err) -> fail ("malformed Execute: " <> err)
    Right (_, _, msg) -> pure msg
  where
    go = do
      portal <- getCString
      limit  <- getInt32be
      pure (ExecuteMsg (decodeUtf8 portal) (fromIntegral limit))

-- | Close payload: kind-byte ('S' or 'P') + name\0
parseCloseFE :: ByteString -> Get FrontendMessage
parseCloseFE payload =
  case runGetOrFail go (BL.fromStrict payload) of
    Left  (_, _, err) -> fail ("malformed Close: " <> err)
    Right (_, _, msg) -> pure msg
  where
    go = do
      kind <- getWord8
      name <- getCString
      pure (CloseMsg (toEnum (fromIntegral kind)) (decodeUtf8 name))
