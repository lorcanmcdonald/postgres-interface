{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.PostgresInterface.Queryable
  ( Queryable (..)
  , ColumnType (..)
  , ColumnValue (..)
  , ColumnName
  , Schema
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Data.Scientific (Scientific)
import Data.Int (Int32, Int64)

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
  toRow  :: a -> [ColumnValue]
