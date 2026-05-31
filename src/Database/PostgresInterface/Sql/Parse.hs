{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Parse
  ( parseSql
  , SqlAst
  ) where

import Data.Text (Text)
import Language.SQL.SimpleSQL.Dialect (Dialect (..), postgres)
import Language.SQL.SimpleSQL.Parse (parseQueryExpr)
import Language.SQL.SimpleSQL.Syntax (QueryExpr)

-- | Re-export the SimpleSQL AST type under our own alias
type SqlAst = QueryExpr

-- | Dialect based on postgres but with SQL type-name keywords also allowed as
-- identifiers. This lets columns named 'date', 'time', 'timestamp', etc. appear
-- unquoted in SELECT lists, WHERE clauses, and ORDER BY clauses.
piDialect :: Dialect
piDialect = postgres
  { diIdentifierKeywords = diIdentifierKeywords postgres <>
      [ "date", "time", "timestamp", "interval"
      , "boolean", "integer", "numeric", "decimal"
      , "year", "month", "day", "hour", "minute", "second", "zone"
      ]
  }

-- | Parse a SQL string into an AST, rejecting non-SELECT statements.
parseSql :: Text -> Either String SqlAst
parseSql sql =
  case parseQueryExpr piDialect "<query>" Nothing sql of
    Left _err -> Left "SQL parse error"
    Right ast -> Right ast
