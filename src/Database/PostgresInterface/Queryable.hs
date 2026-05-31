{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Database.PostgresInterface.Queryable
  ( Queryable (..)
  , ColumnType (..)
  , ColumnValue (..)
  , ColumnName
  , Schema
  -- Generic machinery (re-exported so users only need this module)
  , GSchema (..)
  , GToRow (..)
  , GColumnType (..)
  , GToColumnValue (..)
  , ToColumnType (..)
  , ToColumnValue (..)
  ) where

import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics

type ColumnName = Text
type Schema = [(ColumnName, ColumnType)]

data ColumnType
  = PgText
  | PgInt4
  | PgInt8
  | PgNumeric
  | PgDate
  | PgBool
  deriving (Eq, Show)

data ColumnValue
  = PgTextVal    Text
  | PgInt4Val    Int32
  | PgInt8Val    Int64
  | PgNumericVal Scientific
  | PgDateVal    Day
  | PgBoolVal    Bool
  | PgNull
  deriving (Eq, Show)

class Queryable a where
  schema :: Schema
  default schema :: (Generic a, GSchema (Rep a)) => Schema
  schema = gSchema @(Rep a)

  toRow :: a -> [ColumnValue]
  default toRow :: (Generic a, GToRow (Rep a)) => a -> [ColumnValue]
  toRow x = gToRow (from x)

-- Generic schema extraction ---------------------------------------------------

class GSchema f where
  gSchema :: [(Text, ColumnType)]

instance GSchema f => GSchema (M1 D meta f) where
  gSchema = gSchema @f

instance GSchema f => GSchema (M1 C meta f) where
  gSchema = gSchema @f

instance (Selector s, GColumnType f) => GSchema (M1 S s f) where
  gSchema = [(fieldName', colType)]
    where
      fieldName' = extractFieldLabel (selName (undefined :: M1 S s f p))
      colType    = gColumnType @f

-- | Extract the source-level field name from a GHC.Generics selector name.
-- With DuplicateRecordFields enabled, GHC mangles selectors to
-- "$sel:fieldname:Constructor". This strips the mangling.
extractFieldLabel :: String -> Text
extractFieldLabel ('$':'s':'e':'l':':':rest) = T.pack (takeWhile (/= ':') rest)
extractFieldLabel name                       = T.pack name

instance (GSchema l, GSchema r) => GSchema (l :*: r) where
  gSchema = gSchema @l <> gSchema @r

-- Generic row extraction ------------------------------------------------------

class GToRow f where
  gToRow :: f p -> [ColumnValue]

instance GToRow f => GToRow (M1 D meta f) where
  gToRow (M1 x) = gToRow x

instance GToRow f => GToRow (M1 C meta f) where
  gToRow (M1 x) = gToRow x

instance GToColumnValue f => GToRow (M1 S meta f) where
  gToRow (M1 x) = [gToColumnValue x]

instance (GToRow l, GToRow r) => GToRow (l :*: r) where
  gToRow (l :*: r) = gToRow l <> gToRow r

-- Column type mapping ---------------------------------------------------------

class GColumnType f where
  gColumnType :: ColumnType

instance ToColumnType a => GColumnType (K1 R a) where
  gColumnType = columnType @a

class GToColumnValue f where
  gToColumnValue :: f p -> ColumnValue

instance ToColumnValue a => GToColumnValue (K1 R a) where
  gToColumnValue (K1 x) = toColumnValue x

class ToColumnType a where
  columnType :: ColumnType

class ToColumnValue a where
  toColumnValue :: a -> ColumnValue

instance ToColumnType Text       where columnType = PgText
instance ToColumnType Int32      where columnType = PgInt4
instance ToColumnType Int64      where columnType = PgInt8
instance ToColumnType Scientific where columnType = PgNumeric
instance ToColumnType Day        where columnType = PgDate
instance ToColumnType Bool       where columnType = PgBool

instance ToColumnValue Text       where toColumnValue = PgTextVal
instance ToColumnValue Int32      where toColumnValue = PgInt4Val
instance ToColumnValue Int64      where toColumnValue = PgInt8Val
instance ToColumnValue Scientific where toColumnValue = PgNumericVal
instance ToColumnValue Day        where toColumnValue = PgDateVal
instance ToColumnValue Bool       where toColumnValue = PgBoolVal
