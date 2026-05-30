module Database.PostgresInterface.Sql.Plan
  ( QueryPlan (..)
  , ColumnSelection (..)
  , Predicate (..)
  , CompOp (..)
  , ScalarLiteral (..)
  , SortSpec (..)
  , SortDir (..)
  , QueryError (..)
  , toQueryPlan
  ) where

import Data.Text (Text)
import Data.Time (Day)
import Data.Scientific (Scientific)
import Database.PostgresInterface.Sql.Parse (SqlAst)

type ColumnName = Text

data ColumnSelection
  = AllColumns
  | NamedColumns [ColumnName]
  deriving (Eq, Show)

data SortDir = Asc | Desc
  deriving (Eq, Show)

data SortSpec = SortSpec ColumnName SortDir
  deriving (Eq, Show)

data CompOp = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data ScalarLiteral
  = LitText    Text
  | LitInt     Int
  | LitDate    Day
  | LitNum     Scientific
  deriving (Eq, Show)

data Predicate
  = ColOp ColumnName CompOp ScalarLiteral
  | And   Predicate  Predicate
  | Or    Predicate  Predicate
  deriving (Eq, Show)

data QueryPlan = QueryPlan
  { qpTable      :: ColumnName      -- target table name
  , qpColumns    :: ColumnSelection
  , qpPredicates :: [Predicate]
  , qpOrderBy    :: [SortSpec]
  , qpLimit      :: Maybe Int
  , qpGroupBy    :: [ColumnName]
  }
  deriving (Eq, Show)

data QueryError
  = UnsupportedOperation Text
  | UnknownTable         Text
  | UnknownColumn        Text
  | UnsupportedSyntax    Text
  | InternalError        Text
  deriving (Eq, Show)

-- | Convert a parsed SQL AST into a QueryPlan, or fail with a QueryError
toQueryPlan :: SqlAst -> Either QueryError QueryPlan
toQueryPlan = error "not yet implemented"
