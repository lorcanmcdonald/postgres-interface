{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.PostgresInterface.Table
  ( Table (..)
  , AnyTable (..)
  , anyTable
  , naiveTable
  , dynAnyTable
  , findTable
  , executeTable
  , executeTableRows
  , describeTable
  -- Shared evaluation helpers (used by Table.Dynamic)
  , evalPred
  , evalCompOp
  , applyOp
  , compareColumnValues
  , flipOrd
  , selectColumns
  , encodeValue
  , toFieldInfo
  ) where

import Conduit (ConduitT, runConduit, yieldMany, (.|))
import Conduit qualified as C
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgresInterface.Protocol.Messages
import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Sql.Plan
  ( ColumnSelection (..)
  , CompOp (..)
  , Predicate (..)
  , QueryPlan (..)
  , ScalarLiteral (..)
  , SortDir (..)
  , SortSpec (..)
  )

data Table a = Table
  { tableName    :: Text
  , tableHandler :: QueryPlan -> IO (ConduitT () a IO ())
  }

data AnyTable
  = forall a. Queryable a => AnyTable (Table a)
  | DynTable
      { dynName    :: Text
      , dynSchema  :: Schema
      , dynHandler :: QueryPlan -> IO (ConduitT () [ColumnValue] IO ())
      }

anyTable
  :: Queryable a
  => Text
  -> (QueryPlan -> IO (ConduitT () a IO ()))
  -> AnyTable
anyTable name handler = AnyTable (Table name handler)

-- | Register a dynamic table whose schema is determined at runtime (as a list
-- of (column-name, ColumnType) pairs) rather than via the Queryable typeclass.
-- Rows are produced as [ColumnValue] lists matching the schema order.
dynAnyTable
  :: Text
  -> Schema
  -> (QueryPlan -> IO (ConduitT () [ColumnValue] IO ()))
  -> AnyTable
dynAnyTable = DynTable

-- | Register a table backed by a plain stream.
-- The library applies WHERE predicates, ORDER BY sorting (materialising all rows),
-- and LIMIT truncation automatically from the QueryPlan.
naiveTable
  :: forall a. Queryable a
  => Text
  -> IO (ConduitT () a IO ())
  -> AnyTable
naiveTable name stream = anyTable name $ \plan -> do
  src <- stream
  let sch   = schema @a
      preds = qpPredicates plan
      sorts = qpOrderBy plan
      lim   = qpLimit plan
  pure (applyNaivePlan sch preds sorts lim src)

applyNaivePlan
  :: Queryable a
  => Schema
  -> [Predicate]
  -> [SortSpec]
  -> Maybe Int
  -> ConduitT () a IO ()
  -> ConduitT () a IO ()
applyNaivePlan sch preds sorts lim src = do
  -- 1. Collect all rows (needed for sort; filter first to reduce memory)
  rows <- C.lift $ runConduit (src .| C.filterC (rowMatchesPreds sch preds) .| C.sinkList)
  -- 2. Sort (requires materialisation)
  let sorted = applySort sch sorts rows
  -- 3. Apply LIMIT and yield
  let limited = maybe id take lim sorted
  yieldMany limited

-- Predicate evaluation --------------------------------------------------------

rowMatchesPreds :: Queryable a => Schema -> [Predicate] -> a -> Bool
rowMatchesPreds sch preds row =
  let vals = zip (map fst sch) (toRow row)
  in all (evalPred vals) preds

evalPred :: [(Text, ColumnValue)] -> Predicate -> Bool
evalPred vals (And l r) = evalPred vals l && evalPred vals r
evalPred vals (Or  l r) = evalPred vals l || evalPred vals r
evalPred vals (ColOp col op lit) =
  case lookup col vals of
    Nothing  -> False
    Just val -> evalCompOp op val lit

evalCompOp :: CompOp -> ColumnValue -> ScalarLiteral -> Bool
evalCompOp op (PgInt4Val a)    (LitInt b)  = applyOp op a (fromIntegral b)
evalCompOp op (PgInt8Val a)    (LitInt b)  = applyOp op a (fromIntegral b)
evalCompOp op (PgInt4Val a)    (LitNum b)  = applyOp op (toScientific a) b
evalCompOp op (PgInt8Val a)    (LitNum b)  = applyOp op (toScientific a) b
evalCompOp op (PgNumericVal a) (LitNum b)  = applyOp op a b
evalCompOp op (PgTextVal a)    (LitText b) = applyOp op a b
evalCompOp op (PgDateVal a)    (LitDate b) = applyOp op a b
evalCompOp _ _ _ = False

toScientific :: (Integral a) => a -> Scientific
toScientific = fromIntegral

applyOp :: Ord b => CompOp -> b -> b -> Bool
applyOp Eq a b = a == b
applyOp Ne a b = a /= b
applyOp Lt a b = a <  b
applyOp Le a b = a <= b
applyOp Gt a b = a >  b
applyOp Ge a b = a >= b

-- Sorting ---------------------------------------------------------------------

applySort :: Queryable a => Schema -> [SortSpec] -> [a] -> [a]
applySort _   []    rows = rows
applySort sch specs rows = sortBy (compareBySpecs sch specs) rows

compareBySpecs :: Queryable a => Schema -> [SortSpec] -> a -> a -> Ordering
compareBySpecs sch specs x y =
  let xVals = zip (map fst sch) (toRow x)
      yVals = zip (map fst sch) (toRow y)
  in foldMap (compareBySpec xVals yVals) specs

compareBySpec :: [(Text, ColumnValue)] -> [(Text, ColumnValue)] -> SortSpec -> Ordering
compareBySpec xVals yVals (SortSpec col dir) =
  let base = compareColumnValues (lookup col xVals) (lookup col yVals)
  in case dir of
       Asc  -> base
       Desc -> flipOrd base

flipOrd :: Ordering -> Ordering
flipOrd LT = GT
flipOrd GT = LT
flipOrd EQ = EQ

compareColumnValues :: Maybe ColumnValue -> Maybe ColumnValue -> Ordering
compareColumnValues (Just (PgInt4Val a)) (Just (PgInt4Val b)) = compare a b
compareColumnValues (Just (PgInt8Val a)) (Just (PgInt8Val b)) = compare a b
compareColumnValues (Just (PgTextVal a)) (Just (PgTextVal b)) = compare a b
compareColumnValues (Just (PgDateVal a)) (Just (PgDateVal b)) = compare a b
compareColumnValues (Just (PgBoolVal a)) (Just (PgBoolVal b)) = compare a b
compareColumnValues Nothing              Nothing               = EQ
compareColumnValues Nothing              _                     = LT
compareColumnValues _                    Nothing               = GT
compareColumnValues _                    _                     = EQ

findTable :: Text -> [AnyTable] -> Maybe AnyTable
findTable name = foldr check Nothing
  where
    check t@(AnyTable tbl) acc
      | tableName tbl == name = Just t
      | otherwise             = acc
    check t@(DynTable n _ _) acc
      | n == name = Just t
      | otherwise = acc

-- | Compute RowDescription field descriptors for a table without executing the query.
-- Used by the extended query protocol Describe handler.
describeTable :: AnyTable -> QueryPlan -> [FieldInfo]
describeTable (AnyTable tbl) plan =
  let fullSchema   = anyTableSchema tbl
      activeSchema = selectColumns (qpColumns plan) fullSchema
  in map toFieldInfo activeSchema
describeTable (DynTable _ sch _) plan =
  let activeSchema = selectColumns (qpColumns plan) sch
  in map toFieldInfo activeSchema

-- | Execute a query plan, returning RowDescription + DataRows + CommandComplete.
-- Use for the Simple Query protocol where schema is sent inline with results.
executeTable :: AnyTable -> QueryPlan -> IO [BackendMessage]
executeTable tbl plan = do
  rows   <- executeTableRows tbl plan
  let fields = describeTable tbl plan
  pure $ RowDescription fields : rows

-- | Execute a query plan, returning only DataRows + CommandComplete.
-- Use for the Extended Query protocol Execute step: the client already received
-- RowDescription from the preceding Describe step and must not receive it again.
executeTableRows :: AnyTable -> QueryPlan -> IO [BackendMessage]
executeTableRows (AnyTable tbl) plan = do
  source <- tableHandler tbl plan
  let fullSchema   = anyTableSchema tbl
      activeSchema = selectColumns (qpColumns plan) fullSchema
  rows <- collectRows source (qpLimit plan)
  let dataRows = map (mkDataRow fullSchema activeSchema) rows
      tag      = "SELECT " <> T.pack (show (length dataRows))
  pure $ dataRows ++ [CommandComplete tag]
executeTableRows (DynTable _ sch handler) plan = do
  source <- handler plan
  let activeSchema = selectColumns (qpColumns plan) sch
      activeIdxs   = [ i | (i, (n, _)) <- zip [0..] sch
                         , n `elem` map fst activeSchema ]
  rows <- collectRows source (qpLimit plan)
  let dataRows = map (mkDynDataRow activeIdxs) rows
      tag      = "SELECT " <> T.pack (show (length dataRows))
  pure $ dataRows ++ [CommandComplete tag]

anyTableSchema :: forall b. Queryable b => Table b -> Schema
anyTableSchema _ = schema @b

mkDynDataRow :: [Int] -> [ColumnValue] -> BackendMessage
mkDynDataRow idxs vals =
  DataRow (map encodeValue [ v | (i, v) <- zip [0..] vals, i `elem` idxs ])

selectColumns :: ColumnSelection -> Schema -> Schema
selectColumns AllColumns      sch = sch
selectColumns (NamedColumns ns) sch = filter (\(n, _) -> n `elem` ns) sch

toFieldInfo :: (Text, ColumnType) -> FieldInfo
toFieldInfo (name, colType) = FieldInfo
  { fieldName    = name
  , fieldTypeOid = columnTypeOid colType
  , fieldFormat  = 0
  }

mkDataRow :: Queryable b => Schema -> Schema -> b -> BackendMessage
mkDataRow fullSch activeSch row =
  let allVals   = zip (map fst fullSch) (toRow row)
      projected = [ v | (n, v) <- allVals, n `elem` map fst activeSch ]
  in DataRow (map encodeValue projected)

collectRows :: ConduitT () a IO () -> Maybe Int -> IO [a]
collectRows source Nothing  = runConduit (source .| C.sinkList)
collectRows source (Just n) = runConduit (source .| C.takeC n .| C.sinkList)

encodeValue :: ColumnValue -> Maybe ByteString
encodeValue PgNull           = Nothing
encodeValue (PgTextVal t)    = Just (encodeUtf8 t)
encodeValue (PgInt4Val i)    = Just (encodeUtf8 (T.pack (show i)))
encodeValue (PgInt8Val i)    = Just (encodeUtf8 (T.pack (show i)))
encodeValue (PgNumericVal n) = Just (encodeUtf8 (T.pack (show n)))
encodeValue (PgDateVal d)    = Just (encodeUtf8 (T.pack (show d)))
encodeValue (PgBoolVal b)    = Just (if b then "t" else "f")

columnTypeOid :: ColumnType -> Int32
columnTypeOid PgText    = 25
columnTypeOid PgInt4    = 23
columnTypeOid PgInt8    = 20
columnTypeOid PgNumeric = 1700
columnTypeOid PgDate    = 1082
columnTypeOid PgBool    = 16
