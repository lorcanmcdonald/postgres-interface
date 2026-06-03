{-# LANGUAGE OverloadedStrings #-}

-- | Dynamic table creation: define tables from a runtime schema (list of
-- column definitions) without needing a Haskell record type or Queryable
-- instance. Rows are produced as @[ColumnValue]@ lists.
--
-- Intended for use-cases like scamall's @PostgresTableSchema@ CRD, where the
-- column definitions are known at runtime rather than compile time.
module Database.PostgresInterface.Table.Dynamic
  ( ColumnDef (..)
  , DynColumnType (..)
  , dynamicTable
  , naiveDynamicTable
  ) where

import Conduit (ConduitT, runConduit, yieldMany, (.|))
import Conduit qualified as C
import Data.Text (Text)
import Database.PostgresInterface.Queryable (ColumnType (..), ColumnValue (..), Schema)
import Database.PostgresInterface.Sql.Plan
  ( Predicate (..)
  , QueryPlan (..)
  , SortDir (..)
  , SortSpec (..)
  )
import Database.PostgresInterface.Table
  ( AnyTable
  , dynAnyTable
  , evalPred
  , compareColumnValues
  , flipOrd
  )
import Data.List (sortBy)

-- | A column definition mirroring scamall's @ColumnDef@ CRD struct.
data ColumnDef = ColumnDef
  { colName     :: Text
  , colType     :: DynColumnType
  , colNullable :: Bool
  } deriving (Show)

-- | The column types supported by the dynamic table interface.
-- Maps to the @SimpleColumnType@ / @ColumnType@ enum in scamall's pg-operator.
data DynColumnType
  = DynText
  | DynVarchar Int   -- ^ VARCHAR(n)
  | DynSmallInt
  | DynInteger
  | DynBigInt
  | DynBoolean
  | DynDate
  | DynNumeric
  deriving (Show)

-- | Convert a 'DynColumnType' to the wire-protocol 'ColumnType'.
toColumnType :: DynColumnType -> ColumnType
toColumnType DynText         = PgText
toColumnType (DynVarchar _)  = PgText
toColumnType DynSmallInt     = PgInt4
toColumnType DynInteger      = PgInt4
toColumnType DynBigInt       = PgInt8
toColumnType DynBoolean      = PgBool
toColumnType DynDate         = PgDate
toColumnType DynNumeric      = PgNumeric

-- | Build the wire-level 'Schema' from a list of 'ColumnDef's.
defsToSchema :: [ColumnDef] -> Schema
defsToSchema = map (\cd -> (colName cd, toColumnType (colType cd)))

-- | Register a dynamic table whose handler receives the full 'QueryPlan' and
-- returns a stream of rows (each row is a @[ColumnValue]@ in schema order).
--
-- Predicate evaluation, sorting, and limit are the handler's responsibility.
-- Use 'naiveDynamicTable' for the automatic path.
dynamicTable
  :: [ColumnDef]
  -> Text
  -> (QueryPlan -> IO (ConduitT () [ColumnValue] IO ()))
  -> AnyTable
dynamicTable defs name handler =
  dynAnyTable name (defsToSchema defs) handler

-- | Register a dynamic table backed by a plain stream of rows.
-- The library applies WHERE predicates, ORDER BY sorting (materialising all
-- rows), and LIMIT truncation automatically from the 'QueryPlan'.
naiveDynamicTable
  :: [ColumnDef]
  -> Text
  -> IO (ConduitT () [ColumnValue] IO ())
  -> AnyTable
naiveDynamicTable defs name stream =
  let sch = defsToSchema defs
  in dynAnyTable name sch $ \plan -> do
       src <- stream
       let preds = qpPredicates plan
           sorts = qpOrderBy plan
           lim   = qpLimit plan
       pure (applyDynNaivePlan sch preds sorts lim src)

-- ---------------------------------------------------------------------------
-- Naive plan application for dynamic rows

applyDynNaivePlan
  :: Schema
  -> [Predicate]
  -> [SortSpec]
  -> Maybe Int
  -> ConduitT () [ColumnValue] IO ()
  -> ConduitT () [ColumnValue] IO ()
applyDynNaivePlan sch preds sorts lim src = do
  rows <- C.lift $ runConduit
    (src .| C.filterC (dynRowMatchesPreds sch preds) .| C.sinkList)
  let sorted  = applyDynSort sch sorts rows
      limited = maybe id take lim sorted
  yieldMany limited

dynRowMatchesPreds :: Schema -> [Predicate] -> [ColumnValue] -> Bool
dynRowMatchesPreds sch preds row =
  let vals = zip (map fst sch) row
  in all (evalPred vals) preds

applyDynSort :: Schema -> [SortSpec] -> [[ColumnValue]] -> [[ColumnValue]]
applyDynSort _   []    rows = rows
applyDynSort sch specs rows = sortBy (compareDynBySpecs sch specs) rows

compareDynBySpecs :: Schema -> [SortSpec] -> [ColumnValue] -> [ColumnValue] -> Ordering
compareDynBySpecs sch specs x y =
  let xVals = zip (map fst sch) x
      yVals = zip (map fst sch) y
  in foldMap (compareDynBySpec xVals yVals) specs

compareDynBySpec
  :: [(Text, ColumnValue)]
  -> [(Text, ColumnValue)]
  -> SortSpec
  -> Ordering
compareDynBySpec xVals yVals (SortSpec col dir) =
  let base = compareColumnValues (lookup col xVals) (lookup col yVals)
  in case dir of
       Asc  -> base
       Desc -> flipOrd base
