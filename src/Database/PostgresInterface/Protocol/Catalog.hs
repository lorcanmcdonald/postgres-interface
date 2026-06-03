{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Protocol.Catalog
  ( handleCatalogQuery
  ) where

import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgresInterface.Protocol.Messages

-- | Try to handle a pg_catalog / system query.
-- Returns Just messages if this is a known catalog query, Nothing otherwise.
handleCatalogQuery :: Text -> Maybe [BackendMessage]
handleCatalogQuery sql =
  let normalised = T.strip (T.map toLower sql)
  in matchCatalog normalised

matchCatalog :: Text -> Maybe [BackendMessage]
matchCatalog sql
  -- Empty query
  | T.null sql || sql == ";"
  = Just [EmptyQueryResponse]

  -- SET statements — silently succeed
  | "set " `T.isPrefixOf` sql
  = Just [CommandComplete "SET"]

  -- SELECT version()
  | sql `matches` ["select version()"]
  = Just $ rowResult [FieldInfo "version" 25 0]
                     [[textVal "PostgreSQL 14.0 (postgres-interface)"]]
                     "SELECT 1"

  -- SELECT current_schema()
  | sql `matches` ["select current_schema()"]
  = Just $ rowResult [FieldInfo "current_schema" 25 0]
                     [[textVal "public"]]
                     "SELECT 1"

  -- SHOW transaction_isolation
  | sql `matches` ["show transaction_isolation"]
  = Just $ rowResult [FieldInfo "transaction_isolation" 25 0]
                     [[textVal "read committed"]]
                     "SELECT 1"

  -- SHOW server_version
  | sql `matches` ["show server_version"]
  = Just $ rowResult [FieldInfo "server_version" 25 0]
                     [[textVal "14.0"]]
                     "SELECT 1"

  -- SHOW server_version_num
  | sql `matches` ["show server_version_num"]
  = Just $ rowResult [FieldInfo "server_version_num" 25 0]
                     [[textVal "140000"]]
                     "SELECT 1"

  -- SELECT current_setting('server_version_num') — used by Grafana/pgx for version detection
  -- Returns server_version_num (140000). The ::int/100 cast/division happens client-side
  -- after we return the text value, so we just return the raw number string.
  | "current_setting" `T.isInfixOf` sql && "server_version_num" `T.isInfixOf` sql
  = Just $ rowResult [FieldInfo "version" 23 0]
                     [[textVal "1400"]]
                     "SELECT 1"

  -- SELECT current_database()
  | sql `matches` ["select current_database()"]
  = Just $ rowResult [FieldInfo "current_database" 25 0]
                     [[textVal "postgres"]]
                     "SELECT 1"

  -- pg_catalog introspection — return empty result sets with correct column shapes
  | "pg_type" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "oid" 26 0, FieldInfo "typname" 19 0]

  | "pg_namespace" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "oid" 26 0, FieldInfo "nspname" 19 0]

  | "pg_class" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "oid" 26 0, FieldInfo "relname" 19 0]

  | "pg_attribute" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "attname" 19 0, FieldInfo "atttypid" 26 0]

  | "pg_extension" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "extname" 19 0, FieldInfo "extversion" 25 0]

  | "information_schema" `T.isInfixOf` sql
  = Just $ emptyResult [FieldInfo "table_name" 25 0, FieldInfo "table_schema" 25 0]

  | otherwise
  = Nothing

-- Helpers ---------------------------------------------------------------------

matches :: Text -> [Text] -> Bool
matches sql candidates = any (== sql) candidates

textVal :: Text -> Maybe ByteString
textVal = Just . encodeUtf8

rowResult :: [FieldInfo] -> [[Maybe ByteString]] -> Text -> [BackendMessage]
rowResult fields rows tag =
  [RowDescription fields]
  ++ map DataRow rows
  ++ [CommandComplete tag]

emptyResult :: [FieldInfo] -> [BackendMessage]
emptyResult fields = [RowDescription fields, CommandComplete "SELECT 0"]
