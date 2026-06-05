{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module K8sSpec (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Database.PostgresInterface.K8s
import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Queryable.Generic ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- ---------------------------------------------------------------------------
-- Test types

data Employee = Employee
  { employeeName :: Text
  , salary       :: Int32
  , active       :: Bool
  , hireDate     :: Day
  } deriving (Generic)

instance Queryable Employee
instance ToK8sManifests Employee

-- ---------------------------------------------------------------------------
-- Helpers

manifestAt :: Int -> [K8sManifest] -> Aeson.Value
manifestAt i ms = let K8sManifest v = ms !! i in v

field :: Aeson.Value -> [Text] -> Maybe Aeson.Value
field v []     = Just v
field (Aeson.Object o) (k:ks) =
  case KeyMap.lookup (Key.fromText k) o of
    Nothing -> Nothing
    Just v' -> field v' ks
field _ _ = Nothing

asText :: Aeson.Value -> Maybe Text
asText (Aeson.String t) = Just t
asText _                = Nothing

asInt :: Aeson.Value -> Maybe Int
asInt (Aeson.Number n) = Just (round n)
asInt _                = Nothing

asArray :: Aeson.Value -> Maybe [Aeson.Value]
asArray (Aeson.Array a) = Just (foldr (:) [] a)
asArray _               = Nothing

-- ---------------------------------------------------------------------------
-- Tests

tests :: TestTree
tests = testGroup "K8s"
  [ testGroup "toK8sManifests produces two documents"
      [ testCase "returns exactly 2 manifests" testTwoManifests
      ]
  , testGroup "PostgresTable manifest"
      [ testCase "apiVersion is correct"      testTableApiVersion
      , testCase "kind is PostgresTable"      testTableKind
      , testCase "metadata.name is tableName" testTableMetaName
      , testCase "metadata.namespace is set"  testTableNamespace
      , testCase "spec.tableName matches"     testTableSpecName
      , testCase "spec.pgSchema is public"    testTablePgSchema
      ]
  , testGroup "PostgresTableSchema manifest"
      [ testCase "apiVersion is correct"              testSchemaApiVersion
      , testCase "kind is PostgresTableSchema"        testSchemaKind
      , testCase "metadata.name is tableName-v1"      testSchemaMetaName
      , testCase "spec.tableRef.name matches"         testSchemaTableRef
      , testCase "spec.version is 1"                  testSchemaVersion
      , testCase "spec.columns length matches schema" testSchemaColumnCount
      , testCase "column names match schema"          testSchemaColumnNames
      , testCase "column types are mapped"            testSchemaColumnTypes
      ]
  , testGroup "encodeManifest produces valid YAML"
      [ testCase "single manifest round-trips through YAML" testEncodeRoundTrip
      ]
  , testGroup "encodeManifests multi-doc"
      [ testCase "starts with ---"           testMultiDocSeparator
      , testCase "both docs present"         testMultiDocBothDocs
      ]
  ]

tName :: TableName
tName = "employees"

tNs :: K8sNamespace
tNs = "production"

manifests :: [K8sManifest]
manifests = toK8sManifests @Employee tNs tName

tableManifest :: Aeson.Value
tableManifest = manifestAt 0 manifests

schemaManifest :: Aeson.Value
schemaManifest = manifestAt 1 manifests

testTwoManifests :: Assertion
testTwoManifests = length manifests @?= 2

testTableApiVersion :: Assertion
testTableApiVersion =
  (field tableManifest ["apiVersion"] >>= asText)
    @?= Just "postgres.scamall.io/v1alpha1"

testTableKind :: Assertion
testTableKind =
  (field tableManifest ["kind"] >>= asText) @?= Just "PostgresTable"

testTableMetaName :: Assertion
testTableMetaName =
  (field tableManifest ["metadata", "name"] >>= asText) @?= Just "employees"

testTableNamespace :: Assertion
testTableNamespace =
  (field tableManifest ["metadata", "namespace"] >>= asText) @?= Just "production"

testTableSpecName :: Assertion
testTableSpecName =
  (field tableManifest ["spec", "tableName"] >>= asText) @?= Just "employees"

testTablePgSchema :: Assertion
testTablePgSchema =
  (field tableManifest ["spec", "pgSchema"] >>= asText) @?= Just "public"

testSchemaApiVersion :: Assertion
testSchemaApiVersion =
  (field schemaManifest ["apiVersion"] >>= asText)
    @?= Just "postgres.scamall.io/v1alpha1"

testSchemaKind :: Assertion
testSchemaKind =
  (field schemaManifest ["kind"] >>= asText) @?= Just "PostgresTableSchema"

testSchemaMetaName :: Assertion
testSchemaMetaName =
  (field schemaManifest ["metadata", "name"] >>= asText) @?= Just "employees-v1"

testSchemaTableRef :: Assertion
testSchemaTableRef =
  (field schemaManifest ["spec", "tableRef", "name"] >>= asText)
    @?= Just "employees"

testSchemaVersion :: Assertion
testSchemaVersion =
  (field schemaManifest ["spec", "version"] >>= asInt) @?= Just 1

testSchemaColumnCount :: Assertion
testSchemaColumnCount = do
  let cols = field schemaManifest ["spec", "columns"] >>= asArray
  fmap length cols @?= Just 4  -- Employee has 4 fields

testSchemaColumnNames :: Assertion
testSchemaColumnNames = do
  let cols = field schemaManifest ["spec", "columns"] >>= asArray
  let names = cols >>= mapM (\c -> field c ["name"] >>= asText)
  names @?= Just ["employeeName", "salary", "active", "hireDate"]

testSchemaColumnTypes :: Assertion
testSchemaColumnTypes = do
  let cols = field schemaManifest ["spec", "columns"] >>= asArray
  let types = cols >>= mapM (\c -> field c ["type"] >>= asText)
  types @?= Just ["text", "integer", "boolean", "date"]

testEncodeRoundTrip :: Assertion
testEncodeRoundTrip = do
  let K8sManifest v = head manifests
      encoded       = encodeManifest (head manifests)
  case Yaml.decodeEither' encoded of
    Left err -> assertFailure ("YAML decode failed: " <> show err)
    Right v' -> v' @?= v

testMultiDocSeparator :: Assertion
testMultiDocSeparator = do
  let encoded = encodeManifests manifests
  assertBool "starts with ---" ("---" `BS.isPrefixOf` encoded)

testMultiDocBothDocs :: Assertion
testMultiDocBothDocs = do
  let encoded = encodeManifests manifests
  -- Both kinds should appear in the combined output
  case Yaml.decodeAllEither' encoded of
    Left err -> assertFailure ("YAML decode failed: " <> show err)
    Right (vs :: [Aeson.Value]) -> length vs @?= 2
