{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Parse
  ( parseSql
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgresInterface.Sql.Plan (QueryError (..))
import PostgresqlSyntax.Ast (PreparableStmt (..), SelectStmt)
import PostgresqlSyntax.Parsing (preparableStmt, run)

-- | Parse a SQL string into a SelectStmt, or fail with a QueryError.
-- Non-SELECT statements (INSERT, UPDATE, DELETE) are rejected here rather
-- than later in planning, so callers only ever receive a narrowed SELECT AST.
parseSql :: Text -> Either QueryError SelectStmt
parseSql sql = case run preparableStmt sql of
  Left err                       -> Left (UnsupportedSyntax (T.pack err))
  Right (SelectPreparableStmt s) -> Right s
  Right _                        -> Left (UnsupportedOperation "only SELECT statements are supported")
