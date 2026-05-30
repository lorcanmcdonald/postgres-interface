{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Parse
  ( parseSql
  , SqlAst
  ) where

import Data.Text (Text)
import Language.SQL.SimpleSQL.Dialect (postgres)
import Language.SQL.SimpleSQL.Parse (parseQueryExpr)
import Language.SQL.SimpleSQL.Syntax (QueryExpr)

-- | Re-export the SimpleSQL AST type under our own alias
type SqlAst = QueryExpr

-- | Parse a SQL string into an AST, rejecting non-SELECT statements.
parseSql :: Text -> Either String SqlAst
parseSql sql =
  case parseQueryExpr postgres "<query>" Nothing sql of
    Left _err -> Left "SQL parse error"
    Right ast -> Right ast
