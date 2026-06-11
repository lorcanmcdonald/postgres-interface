{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Parse
  ( parseSql
  , SqlAst
  ) where

import Data.Text (Text)
import Language.SQL.SimpleSQL.Dialect (Dialect (..), postgres)
import Language.SQL.SimpleSQL.Parse (parseStatement)
import Language.SQL.SimpleSQL.Syntax (Statement)

-- | Re-export the SimpleSQL AST type under our own alias
type SqlAst = Statement

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

-- | Parse a SQL string into an AST. Accepts any valid SQL statement;
-- unsupported statement types (INSERT, UPDATE, DELETE) are rejected later
-- by 'toQueryPlan' with a "not implemented" error rather than a parse error.
parseSql :: Text -> Either String SqlAst
parseSql sql =
  case parseStatement piDialect "<query>" Nothing sql of
    Left _err -> Left "SQL parse error"
    Right ast -> Right ast
