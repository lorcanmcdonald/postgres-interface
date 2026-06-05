{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate scamall pg-operator Kubernetes CRD manifests from 'Queryable'
-- schemas.
--
-- The pg-operator defines two CRDs:
--
--   * @PostgresTable@ — the table itself (name, pg schema, database, etc.)
--   * @PostgresTableSchema@ — a versioned column definition for a table
--
-- Both are produced by 'toK8sManifests'. The caller decides how to serialise
-- the resulting list of 'K8sManifest' values — one YAML file per manifest, or
-- a single multi-document file separated by @---@.
--
-- Example:
--
-- @
-- data Employee = Employee { employeeName :: Text, salary :: Int32 }
--   deriving (Generic)
--
-- instance Queryable Employee
-- instance ToK8sManifests Employee
--
-- writeFiles :: IO ()
-- writeFiles = do
--   let manifests = toK8sManifests \@Employee (K8sNamespace "default") (TableName "employees")
--   mapM_ (\\(i, m) -> BS.writeFile ("manifest-" <> show i <> ".yaml") (encodeManifest m))
--         (zip [0 ..] manifests)
-- @
module Database.PostgresInterface.K8s
  ( ToK8sManifests (..)
  , K8sManifest (..)
  , TableName (..)
  , K8sNamespace (..)
  , encodeManifest
  , encodeManifests
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Database.PostgresInterface.Queryable
  ( ColumnType (..)
  , Queryable
  , schema
  )

-- ---------------------------------------------------------------------------
-- Newtypes

-- | The name of the Postgres table (used in both CRDs and the resource name).
newtype TableName = TableName Text
  deriving (Show, Eq, Ord, IsString, Semigroup, Monoid)

-- | The Kubernetes namespace to deploy the CRD instances into.
newtype K8sNamespace = K8sNamespace Text
  deriving (Show, Eq, Ord, IsString)

-- | An opaque Kubernetes manifest document, ready to be encoded to YAML.
newtype K8sManifest = K8sManifest Aeson.Value
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Typeclass

-- | Types that can emit the Kubernetes CRD manifests needed to create the
-- corresponding @PostgresTable@ and @PostgresTableSchema@ resources in a
-- scamall pg-operator cluster.
--
-- The default implementation derives both manifests from 'schema', so most
-- users only need:
--
-- @
-- instance ToK8sManifests MyTable
-- @
class Queryable a => ToK8sManifests a where
  toK8sManifests :: K8sNamespace -> TableName -> [K8sManifest]
  toK8sManifests ns tName =
    [ postgresTableManifest ns tName
    , postgresTableSchemaManifest ns tName (schema @a)
    ]

-- ---------------------------------------------------------------------------
-- Serialisation

-- | Encode a single 'K8sManifest' to YAML bytes.
encodeManifest :: K8sManifest -> ByteString
encodeManifest (K8sManifest v) = Yaml.encode v

-- | Encode a list of 'K8sManifest' values as a single multi-document YAML
-- file, with each document separated by @---@.
encodeManifests :: [K8sManifest] -> ByteString
encodeManifests = foldMap (\m -> "---\n" <> encodeManifest m)

-- ---------------------------------------------------------------------------
-- Manifest builders

postgresTableManifest :: K8sNamespace -> TableName -> K8sManifest
postgresTableManifest (K8sNamespace ns) (TableName tName) =
  K8sManifest $ Aeson.object
    [ "apiVersion" .= ("postgres.scamall.io/v1alpha1" :: Text)
    , "kind"       .= ("PostgresTable" :: Text)
    , "metadata"   .= Aeson.object
        [ "name"      .= tName
        , "namespace" .= ns
        ]
    , "spec" .= Aeson.object
        [ "tableName"       .= tName
        , "pgSchema"        .= ("public" :: Text)
        , "onSchemaDeleted" .= ("retain" :: Text)
        ]
    ]

postgresTableSchemaManifest
  :: K8sNamespace -> TableName -> [(Text, ColumnType)] -> K8sManifest
postgresTableSchemaManifest (K8sNamespace ns) (TableName tName) cols =
  K8sManifest $ Aeson.object
    [ "apiVersion" .= ("postgres.scamall.io/v1alpha1" :: Text)
    , "kind"       .= ("PostgresTableSchema" :: Text)
    , "metadata"   .= Aeson.object
        [ "name"      .= (tName <> "-v1")
        , "namespace" .= ns
        ]
    , "spec" .= Aeson.object
        [ "tableRef" .= Aeson.object [ "name" .= tName ]
        , "version"  .= (1 :: Int)
        , "columns"  .= map columnDefValue cols
        ]
    ]

columnDefValue :: (Text, ColumnType) -> Aeson.Value
columnDefValue (name, colType) = Aeson.object
  [ "name"     .= name
  , "type"     .= columnTypeName colType
  , "nullable" .= False
  ]

columnTypeName :: ColumnType -> Text
columnTypeName PgText    = "text"
columnTypeName PgInt4    = "integer"
columnTypeName PgInt8    = "bigint"
columnTypeName PgNumeric = "numeric"
columnTypeName PgDate    = "date"
columnTypeName PgBool    = "boolean"
