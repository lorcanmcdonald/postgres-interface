{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Protocol.Server
  ( runConnection
  ) where

import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Database.PostgresInterface.Protocol.Catalog (handleCatalogQuery)
import Database.PostgresInterface.Protocol.Decoder (decodeFrontendMessage, decodeStartup)
import Database.PostgresInterface.Protocol.Encoder (encodeBackendMessage)
import Database.PostgresInterface.Protocol.Messages
import Database.PostgresInterface.Sql.Parse (parseSql)
import Database.PostgresInterface.Sql.Plan (QueryError (..), QueryPlan (..), toQueryPlan)
import Database.PostgresInterface.Table (AnyTable, findTable, executeTable)
import Data.Text (Text)
import Data.Text qualified as T
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

-- | Handle a single client connection: startup handshake then message loop.
runConnection :: [AnyTable] -> Socket -> IO ()
runConnection tables sock = handleConnection `finally` pure ()
  where
    handleConnection = do
      -- Read and handle startup message
      startup <- recvStartup sock
      case decodeStartup startup of
        Left err -> sendError sock (internalError ("bad startup: " <> err))
        Right (StartupMessage _ _params) -> do
          sendAll sock (encodeBackendMessage AuthenticationOk)
          -- Send required parameter statuses
          mapM_ (sendAll sock . encodeBackendMessage) defaultParams
          sendAll sock (encodeBackendMessage (ReadyForQuery Idle))
          -- Enter message loop
          messageLoop tables sock
        Right _ ->
          sendError sock (internalError "unexpected message during startup")

-- | Read a complete startup message, handling the optional SSL negotiation first.
-- SSLRequest: length=8, code=80877103 (0x04D2162F) — respond 'N', then read the real startup.
recvStartup :: Socket -> IO ByteString
recvStartup sock = do
  lenBytes <- recvExact sock 4
  if BS.null lenBytes
    then pure BS.empty  -- client disconnected before sending startup
    else do
      let totalLen = fromIntegral (readInt32BE lenBytes) :: Int
      rest <- recvExact sock (totalLen - 4)
      let msg = lenBytes <> rest
      if isSslRequest msg
        then do
          -- Tell client we don't support SSL; it will retry with a plain startup
          sendAll sock (BS.singleton 0x4E)  -- 'N'
          recvStartup sock
        else pure msg

-- SSLRequest magic: 4-byte length (8) + int32 80877103 (0x04D2162F)
isSslRequest :: ByteString -> Bool
isSslRequest bs =
  BS.length bs == 8 && readInt32BE (BS.drop 4 bs) == 80877103

-- | Main message loop: read messages until Terminate or connection closed
messageLoop :: [AnyTable] -> Socket -> IO ()
messageLoop tables sock = do
  -- All messages after startup: 1 type byte + 4 byte length
  headerBytes <- recvExact sock 5
  if BS.null headerBytes
    then pure ()  -- client disconnected cleanly
    else messageLoop' tables sock headerBytes

messageLoop' :: [AnyTable] -> Socket -> ByteString -> IO ()
messageLoop' tables sock headerBytes = do
  let msgLen = fromIntegral (readInt32BE (BS.drop 1 headerBytes)) :: Int
      payloadLen = msgLen - 4
  payload <- recvExact sock payloadLen
  let raw = headerBytes <> payload
  case decodeFrontendMessage raw of
    Left err -> do
      sendError sock (internalError ("decode error: " <> err))
      messageLoop tables sock
    Right Terminate -> pure ()
    Right (Query sql) -> do
      case handleCatalogQuery sql of
        Just msgs ->
          mapM_ (sendAll sock . encodeBackendMessage) msgs
        Nothing ->
          handleUserQuery tables sock sql
      sendAll sock (encodeBackendMessage (ReadyForQuery Idle))
      messageLoop tables sock
    Right msg ->
      sendError sock (internalError ("unexpected message: " <> show msg))


handleUserQuery :: [AnyTable] -> Socket -> Text -> IO ()
handleUserQuery tables sock sql =
  case parseSql sql of
    Left _ ->
      sendError sock (PgError SeverityError "42601" ("syntax error: " <> sql))
    Right ast ->
      case toQueryPlan ast of
        Left err ->
          sendError sock (queryErrorToPgError err)
        Right plan ->
          case findTable (qpTable plan) tables of
            Nothing ->
              sendError sock (PgError SeverityError "42P01"
                ("relation \"" <> qpTable plan <> "\" does not exist"))
            Just tbl -> do
              msgs <- executeTable tbl plan
              mapM_ (sendAll sock . encodeBackendMessage) msgs

queryErrorToPgError :: QueryError -> PgError
queryErrorToPgError (UnsupportedOperation msg) = PgError SeverityError "0A000" msg
queryErrorToPgError (UnknownTable name)         = PgError SeverityError "42P01" ("relation \"" <> name <> "\" does not exist")
queryErrorToPgError (UnknownColumn name)        = PgError SeverityError "42703" ("column \"" <> name <> "\" does not exist")
queryErrorToPgError (UnsupportedSyntax msg)     = PgError SeverityError "42601" msg
queryErrorToPgError (InternalError msg)         = PgError SeverityError "XX000" msg

-- | Receive exactly n bytes, blocking until available
recvExact :: Socket -> Int -> IO ByteString
recvExact _ 0 = pure BS.empty
recvExact sock n = do
  chunk <- recv sock n
  if BS.null chunk
    then pure BS.empty  -- connection closed
    else if BS.length chunk == n
      then pure chunk
      else do
        rest <- recvExact sock (n - BS.length chunk)
        pure (chunk <> rest)

sendError :: Socket -> PgError -> IO ()
sendError sock err = sendAll sock (encodeBackendMessage (ErrorResponse err))

internalError :: String -> PgError
internalError msg = PgError SeverityError "XX000" (T.pack msg)

readInt32BE :: ByteString -> Int
readInt32BE bs =
  fromIntegral (BS.index bs 0) * 16777216
  + fromIntegral (BS.index bs 1) * 65536
  + fromIntegral (BS.index bs 2) * 256
  + fromIntegral (BS.index bs 3)

defaultParams :: [BackendMessage]
defaultParams =
  [ ParameterStatus "server_version"           "14.0"
  , ParameterStatus "server_encoding"          "UTF8"
  , ParameterStatus "client_encoding"          "UTF8"
  , ParameterStatus "DateStyle"                "ISO, MDY"
  , ParameterStatus "integer_datetimes"        "on"
  , ParameterStatus "standard_conforming_strings" "on"
  ]
