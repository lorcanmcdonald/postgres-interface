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
  , executeTable
  , executeTableRows
  , describeTable
  , executeQuery
  , describeQuery
  ) where

import Conduit (ConduitT, runConduit, yieldMany, (.|))
import Conduit qualified as C
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.List (sortBy, tails)
import Data.Maybe (fromMaybe)
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
  , TableSource (..)
  , QueryError (..)
  )

-- A row in the cross-product representation: flat association list of (column, value).
-- Columns appear both unqualified ("col") and qualified ("table.col") so that
-- both simple WHERE predicates and qualified JOIN ON predicates resolve correctly.
type Row = [(Text, ColumnValue)]

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
  rows <- C.lift $ runConduit (src .| C.filterC (rowMatchesPreds sch preds) .| C.sinkList)
  let sorted  = applySort sch sorts rows
      limited = maybe id take lim sorted
  yieldMany limited

-- Predicate evaluation --------------------------------------------------------

rowMatchesPreds :: Queryable a => Schema -> [Predicate] -> a -> Bool
rowMatchesPreds sch preds row =
  let vals = zip (map fst sch) (toRow row)
  in all (evalPred vals) preds

evalPred :: [(Text, ColumnValue)] -> Predicate -> Bool
evalPred vals (And l r)  = evalPred vals l && evalPred vals r
evalPred vals (Or  l r)  = evalPred vals l || evalPred vals r
evalPred vals (Not p)    = not (evalPred vals p)
evalPred vals (ColOp col op lit) =
  case lookup col vals of
    Nothing  -> False
    Just val -> evalCompOp op val lit
evalPred vals (ColCmpCol col1 op col2) =
  case (lookup col1 vals, lookup col2 vals) of
    (Just v1, Just v2) -> evalColCmp op v1 v2
    _                  -> False
evalPred vals (Like col pat) =
  case lookup col vals of
    Just (PgTextVal t) -> matchLike pat t
    _                  -> False
evalPred vals (ILike col pat) =
  case lookup col vals of
    Just (PgTextVal t) -> matchLike (T.toLower pat) (T.toLower t)
    _                  -> False
evalPred vals (IsNull col) =
  lookup col vals == Just PgNull
evalPred vals (IsNotNull col) =
  case lookup col vals of
    Nothing      -> False
    Just PgNull  -> False
    Just _       -> True
evalPred vals (ColIn col lits) =
  case lookup col vals of
    Nothing  -> False
    Just val -> any (evalCompOp Eq val) lits
evalPred vals (ColNotIn col lits) =
  case lookup col vals of
    Nothing  -> False
    Just val -> all (not . evalCompOp Eq val) lits

evalCompOp :: CompOp -> ColumnValue -> ScalarLiteral -> Bool
evalCompOp op (PgInt4Val a)    (LitInt b)  = applyOp op a (fromIntegral b)
evalCompOp op (PgInt8Val a)    (LitInt b)  = applyOp op a (fromIntegral b)
evalCompOp op (PgInt4Val a)    (LitNum b)  = applyOp op (toScientific a) b
evalCompOp op (PgInt8Val a)    (LitNum b)  = applyOp op (toScientific a) b
evalCompOp op (PgNumericVal a) (LitNum b)  = applyOp op a b
evalCompOp op (PgTextVal a)    (LitText b) = applyOp op a b
evalCompOp op (PgDateVal a)    (LitDate b) = applyOp op a b
evalCompOp _ _ _ = False

-- | Evaluate a comparison between two ColumnValues using the Ordering from
-- 'compareColumnValues'.
evalColCmp :: CompOp -> ColumnValue -> ColumnValue -> Bool
evalColCmp op v1 v2 =
  let ord = compareColumnValues (Just v1) (Just v2)
  in case op of
    Eq -> ord == EQ
    Ne -> ord /= EQ
    Lt -> ord == LT
    Le -> ord /= GT
    Gt -> ord == GT
    Ge -> ord /= LT

toScientific :: (Integral a) => a -> Scientific
toScientific = fromIntegral

applyOp :: Ord b => CompOp -> b -> b -> Bool
applyOp Eq a b = a == b
applyOp Ne a b = a /= b
applyOp Lt a b = a <  b
applyOp Le a b = a <= b
applyOp Gt a b = a >  b
applyOp Ge a b = a >= b

-- | Naive SQL LIKE pattern match.  '%' matches any sequence of characters,
-- '_' matches exactly one character; all other characters match literally.
-- O(n²) but correct — suitable for the naive evaluation path.
matchLike :: Text -> Text -> Bool
matchLike pat str = go (T.unpack pat) (T.unpack str)
  where
    go []        []    = True
    go ('%' : ps) s    = any (go ps) (tails s)
    go ('_' : ps) (_:s)= go ps s
    go (p   : ps) (c:s)= p == c && go ps s
    go _         _     = False

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

-- Table lookup ----------------------------------------------------------------

findTable :: Text -> [AnyTable] -> Maybe AnyTable
findTable name = foldr check Nothing
  where
    check t@(AnyTable tbl) acc
      | tableName tbl == name = Just t
      | otherwise             = acc

-- Single-table execution ------------------------------------------------------

-- | Compute RowDescription field descriptors for a single-table query.
describeTable :: AnyTable -> QueryPlan -> [FieldInfo]
describeTable (AnyTable tbl) plan =
  let fullSchema    = anyTableSchema tbl
      activeSchema  = selectColumns (qpColumns plan) fullSchema
      aliases       = qpAliases plan
      applyAlias n  = fromMaybe n (lookup n aliases)
      renamedSchema = [(applyAlias n, ct) | (n, ct) <- activeSchema]
  in map toFieldInfo renamedSchema

-- | Execute a single-table query plan (Simple Query protocol: includes RowDescription).
executeTable :: AnyTable -> QueryPlan -> IO [BackendMessage]
executeTable tbl plan = do
  rows   <- executeTableRows tbl plan
  let fields = describeTable tbl plan
  pure $ RowDescription fields : rows

-- | Execute a single-table query plan (Extended protocol: no RowDescription).
executeTableRows :: AnyTable -> QueryPlan -> IO [BackendMessage]
executeTableRows (AnyTable tbl) plan = do
  source <- tableHandler tbl plan
  let fullSchema   = anyTableSchema tbl
      activeSchema = selectColumns (qpColumns plan) fullSchema
  rows <- collectRows source (qpLimit plan)
  let dataRows = map (mkDataRow fullSchema activeSchema) rows
      tag      = "SELECT " <> T.pack (show (length dataRows))
  pure $ dataRows ++ [CommandComplete tag]

anyTableSchema :: forall b. Queryable b => Table b -> Schema
anyTableSchema _ = schema @b

selectColumns :: ColumnSelection -> Schema -> Schema
selectColumns AllColumns        sch = sch
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

-- Multi-table (cross-product) execution ---------------------------------------

-- | Execute any QueryPlan against a registry of tables.
-- Single-table queries use the typed path; multi-table queries (JOINs,
-- subqueries, comma-lists) use a naive cross-product.
-- Returns Left on a planning error (e.g. unknown table).
executeQuery :: [AnyTable] -> QueryPlan -> IO (Either QueryError [BackendMessage])
executeQuery tables plan = case qpFrom plan of
  [TableRef name _alias] ->
    case findTable name tables of
      Nothing  -> pure (Left (UnknownTable name))
      Just tbl -> Right <$> executeTable tbl plan
  _ -> Right <$> executeMultiTable tables plan

-- | Describe the output schema of any QueryPlan.
describeQuery :: [AnyTable] -> QueryPlan -> Either QueryError [FieldInfo]
describeQuery tables plan = case qpFrom plan of
  [TableRef name _alias] ->
    case findTable name tables of
      Nothing  -> Left (UnknownTable name)
      Just tbl -> Right (describeTable tbl plan)
  _ ->
    let sch      = querySchema tables plan
        selected = selectColumns (qpColumns plan) sch
        aliases  = qpAliases plan
        renamed  = [(fromMaybe n (lookup n aliases), t) | (n, t) <- selected]
    in Right (map toFieldInfo renamed)

executeMultiTable :: [AnyTable] -> QueryPlan -> IO [BackendMessage]
executeMultiTable tables plan = do
  rows <- planRows tables plan
  let sch      = querySchema tables plan
      selected = selectColumns (qpColumns plan) sch
      aliases  = qpAliases plan
      renamed  = [(fromMaybe n (lookup n aliases), t) | (n, t) <- selected]
      fields   = map toFieldInfo renamed
      colNames = map fst (selectColumns (qpColumns plan) sch)
      dataRows = map (mkMapDataRow colNames) rows
      tag      = "SELECT " <> T.pack (show (length dataRows))
  pure $ RowDescription fields : dataRows ++ [CommandComplete tag]

-- | Collect the combined output schema for all table sources in a plan.
querySchema :: [AnyTable] -> QueryPlan -> Schema
querySchema tables plan = concatMap (sourceSchema tables) (qpFrom plan)

sourceSchema :: [AnyTable] -> TableSource -> Schema
sourceSchema tables (TableRef name _alias) =
  case findTable name tables of
    Nothing               -> []
    Just (AnyTable tbl)   -> anyTableSchema tbl
sourceSchema tables (Subquery innerPlan _alias) =
  querySchema tables innerPlan

-- | Execute all table sources and compute the cross product.
planRows :: [AnyTable] -> QueryPlan -> IO [Row]
planRows tables plan = do
  srcRowSets <- mapM (collectSourceRows tables) (qpFrom plan)
  let combined = foldl crossJoin [[]] srcRowSets
      filtered = filter (\r -> all (evalPred r) (qpPredicates plan)) combined
      sorted   = sortByMap (qpOrderBy plan) filtered
  pure (maybe id take (qpLimit plan) sorted)

-- | Collect all rows from a table source as flat association lists.
-- Each row includes both unqualified names (for WHERE predicates) and
-- qualified names prefixed with the table name or alias (for JOIN ON).
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

-- | Add qualified (prefix.col) entries alongside the existing unqualified ones.
addQualified :: Text -> Row -> Row
addQualified prefix row =
  row ++ [(prefix <> "." <> col, val) | (col, val) <- row]

-- | Minimal plan for collecting raw rows from one table (no pushdown).
emptyPlan :: Text -> Maybe Text -> QueryPlan
emptyPlan name alias = QueryPlan
  { qpFrom       = [TableRef name alias]
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

mkMapDataRow :: [Text] -> Row -> BackendMessage
mkMapDataRow cols row =
  DataRow [encodeValue (fromMaybe PgNull (lookup c row)) | c <- cols]
