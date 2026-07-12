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
  , findTable
  , executeQuery
  , describeQuery
  ) where

import Conduit (ConduitT, runConduit, yieldMany, (.|))
import Conduit qualified as C
import Control.Exception (catch, throwIO)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgresInterface.Protocol.Messages
import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Sql.Plan
  ( ColumnSelection (..)
  , Predicate (..)
  , QueryPlan (..)
  , SortDir (..)
  , SortSpec (..)
  , TableSource (..)
  , QueryError (..)
  )
import Database.PostgresInterface.Sql.PredicateEval
  ( EvalError (..)
  , Row
  , compareColumnValues
  , evalPredicates
  , lookupCI
  )

data Table a = Table
  { tableName    :: Text
  , tableHandler :: QueryPlan -> IO (ConduitT () a IO ())
  }

data AnyTable = forall a. Queryable a => AnyTable (Table a)

anyTable
  :: Queryable a
  => Text
  -> (QueryPlan -> IO (ConduitT () a IO ()))
  -> AnyTable
anyTable name handler = AnyTable (Table name handler)

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
  rows <- C.lift $ runConduit (src .| C.sinkList)
  kept <- C.lift $ filterM
    (\r -> either throwIO pure (evalPredicates (zip (map fst sch) (toRow r)) preds))
    rows
  let sorted  = applySort sch sorts kept
      limited = maybe id take lim sorted
  yieldMany limited

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
  let base = compareColumnValues (lookupCI col xVals) (lookupCI col yVals)
  in case dir of
       Asc  -> base
       Desc -> flipOrd base

flipOrd :: Ordering -> Ordering
flipOrd LT = GT
flipOrd GT = LT
flipOrd EQ = EQ

-- Table lookup ----------------------------------------------------------------

findTable :: Text -> [AnyTable] -> Maybe AnyTable
findTable name = foldr check Nothing
  where
    check t@(AnyTable tbl) acc
      | tableName tbl == name = Just t
      | otherwise             = acc

-- Shared helpers --------------------------------------------------------------

anyTableSchema :: forall b. Queryable b => Table b -> Schema
anyTableSchema _ = schema @b

selectColumns :: ColumnSelection -> Schema -> Schema
selectColumns AllColumns        sch = sch
selectColumns (NamedColumns ns) sch =
  let lowerNs = map T.toLower ns
  in filter (\(n, _) -> T.toLower n `elem` lowerNs) sch

toFieldInfo :: (Text, ColumnType) -> FieldInfo
toFieldInfo (name, colType) = FieldInfo
  { fieldName    = name
  , fieldTypeOid = columnTypeOid colType
  , fieldFormat  = 0
  }

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

-- Multi-table (cross-product) helpers -----------------------------------------

querySchema :: [AnyTable] -> QueryPlan -> Schema
querySchema tables plan = concatMap (sourceSchema tables) (qpSources plan)

sourceSchema :: [AnyTable] -> TableSource -> Schema
sourceSchema tables (TableRef name _alias) =
  case findTable name tables of
    Nothing               -> []
    Just (AnyTable tbl)   -> anyTableSchema tbl
sourceSchema tables (Subquery innerPlan _alias) =
  querySchema tables innerPlan

planRows :: [AnyTable] -> QueryPlan -> IO [Row]
planRows tables plan = do
  srcRowSets <- mapM (collectSourceRows tables) (qpSources plan)
  let combined = foldl crossJoin [[]] srcRowSets
  filtered <- filterM
    (\r -> either throwIO pure (evalPredicates r (qpPredicates plan)))
    combined
  let sorted = sortByMap (qpOrderBy plan) filtered
  pure (maybe id take (qpLimit plan) sorted)

collectSourceRows :: [AnyTable] -> TableSource -> IO [Row]
collectSourceRows tables (TableRef name alias) =
  case findTable name tables of
    Nothing -> pure []
    Just (AnyTable tbl) -> do
      let prefix  = fromMaybe name alias
          rawPlan = emptyPlan name alias
      source <- tableHandler tbl rawPlan
      rows   <- runConduit (source .| C.sinkList)
      let sch = anyTableSchema tbl
      pure [ addQualified prefix (zip (map fst sch) (toRow r)) | r <- rows ]
collectSourceRows tables (Subquery innerPlan alias) = do
  rows <- planRows tables innerPlan
  pure [ addQualified alias row | row <- rows ]

addQualified :: Text -> Row -> Row
addQualified prefix row =
  row ++ [(prefix <> "." <> col, val) | (col, val) <- row]

emptyPlan :: Text -> Maybe Text -> QueryPlan
emptyPlan name alias = QueryPlan
  { qpSources    = [TableRef name alias]
  , qpColumns    = AllColumns
  , qpAliases    = []
  , qpPredicates = []
  , qpOrderBy    = []
  , qpLimit      = Nothing
  , qpGroupBy    = []
  }

crossJoin :: [Row] -> [Row] -> [Row]
crossJoin rows1 rows2 = [r1 ++ r2 | r1 <- rows1, r2 <- rows2]

sortByMap :: [SortSpec] -> [Row] -> [Row]
sortByMap []    rows = rows
sortByMap specs rows = sortBy (compareRowsBySpecs specs) rows

compareRowsBySpecs :: [SortSpec] -> Row -> Row -> Ordering
compareRowsBySpecs specs r1 r2 = foldMap (compareRowBySpec r1 r2) specs

compareRowBySpec :: Row -> Row -> SortSpec -> Ordering
compareRowBySpec r1 r2 (SortSpec col dir) =
  let base = compareColumnValues (lookup col r1) (lookup col r2)
  in case dir of
    Asc  -> base
    Desc -> flipOrd base

-- Unified execution -----------------------------------------------------------

evalErrorToQueryError :: EvalError -> QueryError
evalErrorToQueryError (ColumnNotFound col)   = UnknownColumn col
evalErrorToQueryError (TypeMismatch val lit) =
  UnsupportedOperation (T.pack (show val) <> " is incompatible with " <> T.pack (show lit))

applyAliases :: [(Text, Text)] -> Schema -> Schema
applyAliases aliases = map (\(n, ct) -> (fromMaybe n (lookup n aliases), ct))

-- | Derive the output schema for any QueryPlan without executing it.
planSchema :: [AnyTable] -> QueryPlan -> Either QueryError Schema
planSchema tables plan = case qpSources plan of
  [TableRef name _alias] ->
    case findTable name tables of
      Nothing             -> Left (UnknownTable name)
      Just (AnyTable tbl) ->
        Right (applyAliases (qpAliases plan) (selectColumns (qpColumns plan) (anyTableSchema tbl)))
  _ ->
    Right (applyAliases (qpAliases plan) (selectColumns (qpColumns plan) (querySchema tables plan)))

-- | Execute a single-table plan. Passes the full QueryPlan to the handler for
-- potential pushdown. Returns the projected aliased schema and values in schema
-- order for each row.
runSingleTable :: AnyTable -> QueryPlan -> IO (Schema, [[ColumnValue]])
runSingleTable (AnyTable tbl) plan = do
  let fullSch     = anyTableSchema tbl
      activeSch   = selectColumns (qpColumns plan) fullSch
      activeNames = map fst activeSch
      renamedSch  = applyAliases (qpAliases plan) activeSch
  source <- tableHandler tbl plan
  rows   <- collectRows source (qpLimit plan)
  let project r =
        let allVals = zip (map fst fullSch) (toRow r)
        in [v | (n, v) <- allVals, n `elem` activeNames]
  pure (renamedSch, map project rows)

-- | Execute a multi-table cross-product plan. Always uses emptyPlan per source
-- (no pushdown). Returns the projected aliased schema and values in schema order
-- for each row.
runMultiTable :: [AnyTable] -> QueryPlan -> IO (Schema, [[ColumnValue]])
runMultiTable tables plan = do
  rows <- planRows tables plan
  let sch         = querySchema tables plan
      activeSch   = selectColumns (qpColumns plan) sch
      activeNames = map fst activeSch
      renamedSch  = applyAliases (qpAliases plan) activeSch
      project r   = [fromMaybe PgNull (lookup n r) | n <- activeNames]
  pure (renamedSch, map project rows)

-- | Unified executor. Picks single-table or cross-product internally.
-- EvalError (predicate type mismatches, missing columns) is caught and
-- converted to Left QueryError rather than propagating as an IO exception.
runPlan :: [AnyTable] -> QueryPlan -> IO (Either QueryError (Schema, [[ColumnValue]]))
runPlan tables plan = case qpSources plan of
  [TableRef name _alias] ->
    case findTable name tables of
      Nothing  -> pure (Left (UnknownTable name))
      Just tbl -> (Right <$> runSingleTable tbl plan)
                    `catch` \(e :: EvalError) -> pure (Left (evalErrorToQueryError e))
  _ -> (Right <$> runMultiTable tables plan)
         `catch` \(e :: EvalError) -> pure (Left (evalErrorToQueryError e))

-- | Build the full wire message list from an executor result.
buildMessages :: Schema -> [[ColumnValue]] -> [BackendMessage]
buildMessages sch valss =
  let fields   = map toFieldInfo sch
      dataRows = map (DataRow . map encodeValue) valss
      tag      = "SELECT " <> T.pack (show (length valss))
  in RowDescription fields : dataRows ++ [CommandComplete tag]

-- | Execute any QueryPlan against a registry of tables.
-- Single-table queries use the typed path; multi-table queries use a naive
-- cross-product. Returns Left on a planning error (unknown table or predicate
-- type mismatch).
executeQuery :: [AnyTable] -> QueryPlan -> IO (Either QueryError [BackendMessage])
executeQuery tables plan =
  (uncurry buildMessages <$>) <$> runPlan tables plan

-- | Describe the output schema of any QueryPlan.
describeQuery :: [AnyTable] -> QueryPlan -> Either QueryError [FieldInfo]
describeQuery tables plan = fmap (map toFieldInfo) (planSchema tables plan)
