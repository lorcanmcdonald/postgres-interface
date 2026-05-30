module Database.PostgresInterface.Protocol.Messages
  ( BackendMessage (..)
  , FrontendMessage (..)
  , FieldInfo (..)
  , PgError (..)
  , TxStatus (..)
  , ErrorSeverity (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32)
import Data.Text (Text)

data TxStatus = Idle | InTransaction | FailedTransaction
  deriving (Eq, Show)

data ErrorSeverity = SeverityError | SeverityFatal | SeverityPanic
  deriving (Eq, Show)

data PgError = PgError
  { pgErrorSeverity :: ErrorSeverity
  , pgErrorCode     :: Text  -- SQLSTATE 5-char code
  , pgErrorMessage  :: Text
  }
  deriving (Eq, Show)

data FieldInfo = FieldInfo
  { fieldName   :: Text
  , fieldTypeOid :: Int32
  , fieldFormat  :: Int16  -- 0 = text
  }
  deriving (Eq, Show)

-- | Messages sent from server to client
data BackendMessage
  = AuthenticationOk
  | ParameterStatus Text Text        -- key, value
  | ReadyForQuery TxStatus
  | RowDescription [FieldInfo]
  | DataRow [Maybe ByteString]
  | CommandComplete Text             -- e.g. "SELECT 3"
  | ErrorResponse PgError
  | EmptyQueryResponse
  deriving (Eq, Show)

-- | Messages sent from client to server
data FrontendMessage
  = StartupMessage Int32 [(Text, Text)]  -- protocol version, params
  | Query Text                           -- simple query
  | Terminate
  deriving (Eq, Show)
