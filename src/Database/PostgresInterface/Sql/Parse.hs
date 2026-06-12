{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Parse
  ( parseSql
  , SqlAst
  ) where

import Data.Text (Text)
import PostgresqlSyntax.Ast (PreparableStmt)
import PostgresqlSyntax.Parsing (preparableStmt, run)

-- | Re-export the postgresql-syntax AST type under our own alias
type SqlAst = PreparableStmt

-- | Parse a SQL string into an AST. Accepts any valid PostgreSQL statement;
-- unsupported statement types (INSERT, UPDATE, DELETE) are rejected later
-- by 'toQueryPlan' with a "not implemented" error rather than a parse error.
parseSql :: Text -> Either String SqlAst
parseSql sql = run preparableStmt sql
