{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Plan
  ( QueryPlan (..)
  , ColumnSelection (..)
  , TableSource (..)
  , Predicate (..)
  , CompOp (..)
  , ScalarLiteral (..)
  , SortSpec (..)
  , SortDir (..)
  , QueryError (..)
  , toQueryPlan
  ) where

import Data.Foldable (asum, toList)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, LocalTime (..), UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.PostgresInterface.Sql.Parse (SqlAst)
import PostgresqlSyntax.Ast

type ColumnName = Text

data ColumnSelection
  = AllColumns
  | NamedColumns [ColumnName]
  deriving (Eq, Show)

data SortDir = Asc | Desc
  deriving (Eq, Show)

data SortSpec = SortSpec ColumnName SortDir
  deriving (Eq, Show)

data CompOp = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data ScalarLiteral
  = LitText    Text
  | LitInt     Int
  | LitDate    Day
  | LitNum     Scientific
  deriving (Eq, Show)

-- | A single source in the FROM clause.
data TableSource
  = TableRef ColumnName (Maybe ColumnName)  -- ^ table [AS alias]
  | Subquery QueryPlan  ColumnName          -- ^ (SELECT ...) AS alias
  deriving (Eq, Show)

data Predicate
  = ColOp     ColumnName CompOp ScalarLiteral
  | ColCmpCol ColumnName CompOp ColumnName   -- ^ col op col (e.g. JOIN ON t.id = u.id)
  | Like      ColumnName Text                -- ^ col LIKE pattern  (% and _ wildcards)
  | ILike     ColumnName Text                -- ^ col ILIKE pattern (case-insensitive)
  | IsNull    ColumnName
  | IsNotNull ColumnName
  | ColIn     ColumnName [ScalarLiteral]     -- ^ col IN (...)
  | ColNotIn  ColumnName [ScalarLiteral]     -- ^ col NOT IN (...)
  | Not       Predicate
  | And       Predicate Predicate
  | Or        Predicate Predicate
  deriving (Eq, Show)

data QueryPlan = QueryPlan
  { qpSources    :: [TableSource]                  -- ^ table sources (cross joined for multiple)
  , qpColumns    :: ColumnSelection
  , qpAliases    :: [(ColumnName, ColumnName)]      -- ^ (original column name, output alias)
  , qpPredicates :: [Predicate]
  , qpOrderBy    :: [SortSpec]
  , qpLimit      :: Maybe Int
  , qpGroupBy    :: [ColumnName]
  }
  deriving (Eq, Show)

data QueryError
  = UnsupportedOperation Text
  | UnknownTable         Text
  | UnknownColumn        Text
  | UnsupportedSyntax    Text
  | InternalError        Text
  deriving (Eq, Show)

-- | Convert a parsed SQL AST into a QueryPlan, or fail with a QueryError.
toQueryPlan :: SqlAst -> Either QueryError QueryPlan
toQueryPlan (SelectPreparableStmt sel) = toQueryPlanSelectStmt sel
toQueryPlan (InsertPreparableStmt _)   = Left (UnsupportedOperation "INSERT is not supported")
toQueryPlan (UpdatePreparableStmt _)   = Left (UnsupportedOperation "UPDATE is not supported")
toQueryPlan (DeletePreparableStmt _)   = Left (UnsupportedOperation "DELETE is not supported")
toQueryPlan (CallPreparableStmt _)     = Left (UnsupportedOperation "CALL is not supported")

-- SelectStmt = Either SelectNoParens SelectWithParens
toQueryPlanSelectStmt :: SelectStmt -> Either QueryError QueryPlan
toQueryPlanSelectStmt (Left snp)  = toQueryPlanNoParens snp
toQueryPlanSelectStmt (Right swp) = toQueryPlanWithParens swp

toQueryPlanWithParens :: SelectWithParens -> Either QueryError QueryPlan
toQueryPlanWithParens (NoParensSelectWithParens snp) = toQueryPlanNoParens snp
toQueryPlanWithParens (WithParensSelectWithParens swp) = toQueryPlanWithParens swp

-- SelectNoParens = SelectNoParens (Maybe WithClause) SelectClause (Maybe SortClause) (Maybe SelectLimit) (Maybe ForLockingClause)
toQueryPlanNoParens :: SelectNoParens -> Either QueryError QueryPlan
toQueryPlanNoParens (SelectNoParens _ selectClause mSortClause mSelectLimit _) =
  toQueryPlanClause selectClause mSortClause mSelectLimit

-- SelectClause = Either SimpleSelect SelectWithParens
toQueryPlanClause :: SelectClause -> Maybe SortClause -> Maybe SelectLimit -> Either QueryError QueryPlan
toQueryPlanClause (Left ss) mSort mLimit = toQueryPlanSimple ss mSort mLimit
toQueryPlanClause (Right swp) _ _        = toQueryPlanWithParens swp

-- NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause)
--                   (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause)
--                   (Maybe WindowClause)
toQueryPlanSimple :: SimpleSelect -> Maybe SortClause -> Maybe SelectLimit -> Either QueryError QueryPlan
toQueryPlanSimple (NormalSimpleSelect mTargeting _ mFromClause mWhereClause mGroupClause _ _) mSortClause mSelectLimit = do
  (srcs, fromPreds) <- extractFrom (maybe [] toList mFromClause)
  (cols, aliases)   <- extractTargeting mTargeting
  wherePreds        <- maybe (Right []) (\w -> (:[]) <$> extractPredicate w) mWhereClause
  sorts             <- maybe (Right []) (mapM extractSort . toList) mSortClause
  limit             <- maybe (Right Nothing) (fmap Just . extractSelectLimit) mSelectLimit
  groups            <- maybe (Right []) (mapM extractGroupCol . toList) mGroupClause
  pure QueryPlan
    { qpSources    = srcs
    , qpColumns    = cols
    , qpAliases    = aliases
    , qpPredicates = fromPreds ++ wherePreds
    , qpOrderBy    = sorts
    , qpLimit      = limit
    , qpGroupBy    = groups
    }
toQueryPlanSimple _ _ _ =
  Left (UnsupportedSyntax "only simple SELECT statements are supported")

-- FROM extraction -------------------------------------------------------------

extractFrom :: [TableRef] -> Either QueryError ([TableSource], [Predicate])
extractFrom [] = Left (UnsupportedSyntax "no FROM clause")
extractFrom refs = do
  pairs <- mapM extractFromRef refs
  pure (concatMap fst pairs, concatMap snd pairs)

extractFromRef :: TableRef -> Either QueryError ([TableSource], [Predicate])
extractFromRef (RelationExprTableRef relExpr mAlias _) = do
  name <- extractRelationName relExpr
  let alias = (\(AliasClause _ ident _) -> identToText ident) <$> mAlias
  pure ([TableRef name alias], [])
extractFromRef (SelectTableRef _ swp mAlias) = do
  innerPlan <- toQueryPlanWithParens swp
  case mAlias of
    Just (AliasClause _ ident _) -> pure ([Subquery innerPlan (identToText ident)], [])
    Nothing -> Left (UnsupportedSyntax "subquery in FROM must have an alias")
extractFromRef (JoinTableRef jt _) = extractJoinedTable jt
extractFromRef ref =
  Left (UnsupportedSyntax ("unsupported FROM clause: " <> T.pack (show ref)))

extractRelationName :: RelationExpr -> Either QueryError Text
extractRelationName (SimpleRelationExpr qn _) = Right (qualifiedNameToText qn)
extractRelationName (OnlyRelationExpr qn _)   = Right (qualifiedNameToText qn)

qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText (SimpleQualifiedName ident)        = identToText ident
qualifiedNameToText (IndirectedQualifiedName ident ind) =
  T.intercalate "." (identToText ident : [identToText i | AttrNameIndirectionEl i <- toList ind])

identToText :: Ident -> Text
identToText (QuotedIdent t)   = t
identToText (UnquotedIdent t) = t

extractJoinedTable :: JoinedTable -> Either QueryError ([TableSource], [Predicate])
extractJoinedTable (InParensJoinedTable jt) = extractJoinedTable jt
extractJoinedTable (MethJoinedTable meth lhs rhs) = do
  (lhsSrcs, lhsPreds) <- extractFromRef lhs
  (rhsSrcs, rhsPreds) <- extractFromRef rhs
  condPreds <- case meth of
    CrossJoinMeth                          -> Right []
    QualJoinMeth _ (OnJoinQual e)          -> (:[]) <$> extractPredicate e
    QualJoinMeth _ (UsingJoinQual _)       -> Left (UnsupportedSyntax "JOIN USING not supported")
    NaturalJoinMeth _                      -> Left (UnsupportedSyntax "NATURAL JOIN not supported")
  pure (lhsSrcs ++ rhsSrcs, lhsPreds ++ rhsPreds ++ condPreds)

-- Column extraction -----------------------------------------------------------

-- | Returns the column selection and a list of (original, alias) pairs for
-- columns that have an explicit alias different from their source name.
extractTargeting :: Maybe Targeting -> Either QueryError (ColumnSelection, [(ColumnName, ColumnName)])
extractTargeting Nothing                        = Right (AllColumns, [])
extractTargeting (Just (AllTargeting Nothing))  = Right (AllColumns, [])
extractTargeting (Just (AllTargeting (Just tl))) = extractTargetList tl
extractTargeting (Just (NormalTargeting tl))    = extractTargetList tl
extractTargeting (Just (DistinctTargeting _ tl)) = extractTargetList tl

extractTargetList :: TargetList -> Either QueryError (ColumnSelection, [(ColumnName, ColumnName)])
extractTargetList tl
  | [AsteriskTargetEl] <- toList tl = Right (AllColumns, [])
  | otherwise = do
      pairs <- mapM extractTargetEl (toList tl)
      let names   = map fst pairs
          aliases = [(orig, alias) | (orig, alias) <- pairs, orig /= alias]
      pure (NamedColumns names, aliases)

-- | Returns (original column name, output name).
-- For expressions with an alias but no simple column name, uses the alias as
-- both fields (matching the old behaviour for aggregates / arithmetic).
extractTargetEl :: TargetEl -> Either QueryError (ColumnName, ColumnName)
extractTargetEl AsteriskTargetEl =
  Left (UnsupportedSyntax "unexpected * in column list")
extractTargetEl (AliasedExprTargetEl expr ident) =
  let alias = identToText ident
  in case extractExprColName expr of
       Right orig -> pure (orig, alias)
       Left _     -> pure (alias, alias)  -- non-column expr: use alias for both
extractTargetEl (ImplicitlyAliasedExprTargetEl expr ident) =
  let alias = identToText ident
  in case extractExprColName expr of
       Right orig -> pure (orig, alias)
       Left _     -> pure (alias, alias)
extractTargetEl (ExprTargetEl expr) = do
  n <- extractExprColName expr
  pure (n, n)

extractExprColName :: AExpr -> Either QueryError ColumnName
extractExprColName (CExprAExpr (ColumnrefCExpr cr)) = Right (columnrefToText cr)
extractExprColName expr =
  Left (UnsupportedSyntax ("unsupported SELECT expression: " <> T.pack (show expr)))

columnrefToText :: Columnref -> Text
columnrefToText (Columnref ident Nothing) = identToText ident
columnrefToText (Columnref ident (Just ind)) =
  T.intercalate "." (identToText ident : [identToText i | AttrNameIndirectionEl i <- toList ind])

-- Predicate extraction --------------------------------------------------------

extractPredicate :: AExpr -> Either QueryError Predicate
extractPredicate (AndAExpr lhs rhs) =
  And <$> extractPredicate lhs <*> extractPredicate rhs
extractPredicate (OrAExpr lhs rhs) =
  Or <$> extractPredicate lhs <*> extractPredicate rhs
extractPredicate (NotAExpr e) =
  Not <$> extractPredicate e
-- col LIKE / ILIKE / NOT LIKE — Bool arg is True when negated
extractPredicate (VerbalExprBinOpAExpr lhs negated verb rhs _) = do
  col <- extractAExprColName lhs
  case verb of
    LikeVerbalExprBinOp -> do
      pat <- extractPatternText rhs
      pure (if negated then Not (Like col pat) else Like col pat)
    IlikeVerbalExprBinOp ->
      Left (UnsupportedSyntax "ILIKE is not supported")
    SimilarToVerbalExprBinOp ->
      Left (UnsupportedSyntax "SIMILAR TO is not supported")
-- IS NULL / IS NOT NULL / IN / NOT IN / BETWEEN — Bool arg is True when negated
extractPredicate (ReversableOpAExpr expr negated op) =
  case op of
    NullAExprReversableOp -> do
      col <- extractAExprColName expr
      pure (if negated then IsNotNull col else IsNull col)
    InAExprReversableOp (ExprListInExpr exprs) -> do
      col  <- extractAExprColName expr
      lits <- mapM extractAExprLiteral (toList exprs)
      pure (if negated then ColNotIn col lits else ColIn col lits)
    InAExprReversableOp (SelectInExpr _) ->
      Left (UnsupportedSyntax "IN (SELECT ...) not supported")
    BetweenAExprReversableOp _ lo hi -> do
      col   <- extractAExprColName expr
      loLit <- extractBExprLiteral lo
      hiLit <- extractAExprLiteral hi
      let between = And (ColOp col Ge loLit) (ColOp col Le hiLit)
      pure (if negated then Not between else between)
    _ ->
      Left (UnsupportedSyntax ("unsupported reversable predicate: " <> T.pack (show op)))
-- ISNULL / NOTNULL keywords
extractPredicate (IsnullAExpr expr) = IsNull    <$> extractAExprColName expr
extractPredicate (NotnullAExpr expr) = IsNotNull <$> extractAExprColName expr
-- Parenthesised predicate
extractPredicate (CExprAExpr (InParensCExpr inner Nothing)) = extractPredicate inner
-- postgresql-syntax folds AND/OR into the RHS of comparison operators, e.g.
-- "a = 'x' AND b = 'y'" parses as "a = ('x' AND (b = 'y'))".
-- We detect that pattern and reconstruct the correct "(a = 'x') AND (b = 'y')".
extractPredicate (SymbolicBinOpAExpr lhs op@(MathSymbolicExprBinOp _) (AndAExpr andLhs andRhs)) = do
  p1 <- extractPredicate (SymbolicBinOpAExpr lhs op andLhs)
  p2 <- extractPredicate andRhs
  pure (And p1 p2)
extractPredicate (SymbolicBinOpAExpr lhs op@(MathSymbolicExprBinOp _) (OrAExpr orLhs orRhs)) = do
  p1 <- extractPredicate (SymbolicBinOpAExpr lhs op orLhs)
  p2 <- extractPredicate orRhs
  pure (Or p1 p2)
-- col op col  or  col op literal
extractPredicate (SymbolicBinOpAExpr lhs (MathSymbolicExprBinOp mathOp) rhs) = do
  compOp <- parseMathOp mathOp
  case (lhs, rhs) of
    (CExprAExpr (ColumnrefCExpr lCr), CExprAExpr (ColumnrefCExpr rCr)) ->
      pure (ColCmpCol (columnrefToText lCr) compOp (columnrefToText rCr))
    (CExprAExpr (ColumnrefCExpr cr), _) -> do
      lit <- extractAExprLiteral rhs
      pure (ColOp (columnrefToText cr) compOp lit)
    _ ->
      Left (UnsupportedSyntax ("unsupported predicate lhs: " <> T.pack (show lhs)))
extractPredicate expr =
  Left (UnsupportedSyntax ("unsupported predicate: " <> T.pack (show expr)))

extractAExprColName :: AExpr -> Either QueryError ColumnName
extractAExprColName (CExprAExpr (ColumnrefCExpr cr)) = Right (columnrefToText cr)
extractAExprColName expr =
  Left (UnsupportedSyntax ("expected column reference, got: " <> T.pack (show expr)))

extractPatternText :: AExpr -> Either QueryError Text
extractPatternText (CExprAExpr (AexprConstCExpr (SAexprConst s))) = Right s
extractPatternText expr =
  Left (UnsupportedSyntax ("expected string literal for pattern: " <> T.pack (show expr)))

parseMathOp :: MathOp -> Either QueryError CompOp
parseMathOp EqualsMathOp                 = Right Eq
parseMathOp ArrowLeftArrowRightMathOp    = Right Ne  -- <>
parseMathOp ExclamationEqualsMathOp      = Right Ne  -- !=
parseMathOp ArrowLeftMathOp              = Right Lt
parseMathOp LessEqualsMathOp             = Right Le
parseMathOp ArrowRightMathOp             = Right Gt
parseMathOp GreaterEqualsMathOp          = Right Ge
parseMathOp op =
  Left (UnsupportedSyntax ("unsupported operator: " <> T.pack (show op)))

extractAExprLiteral :: AExpr -> Either QueryError ScalarLiteral
extractAExprLiteral (CExprAExpr (AexprConstCExpr c)) = extractConst c
extractAExprLiteral expr =
  Left (UnsupportedSyntax ("unsupported literal: " <> T.pack (show expr)))

extractBExprLiteral :: BExpr -> Either QueryError ScalarLiteral
extractBExprLiteral (CExprBExpr (AexprConstCExpr c)) = extractConst c
extractBExprLiteral expr =
  Left (UnsupportedSyntax ("unsupported literal in BETWEEN: " <> T.pack (show expr)))

extractConst :: AexprConst -> Either QueryError ScalarLiteral
extractConst (SAexprConst s) =
  case parseIsoDate (T.unpack s) of
    Just d  -> Right (LitDate d)
    Nothing -> Right (LitText s)
extractConst (IAexprConst i) = Right (LitNum (fromIntegral i))
extractConst (FAexprConst f) = Right (LitNum (realToFrac f))
extractConst c =
  Left (UnsupportedSyntax ("unsupported constant: " <> T.pack (show c)))

-- | Try to extract a 'Day' from an ISO 8601 string, accepting any valid
-- combination of date, local datetime, or UTC datetime.
parseIsoDate :: String -> Maybe Day
parseIsoDate s = asum
  [ utctDay  <$> (iso8601ParseM s :: Maybe UTCTime)
  , localDay <$> (iso8601ParseM s :: Maybe LocalTime)
  ,               iso8601ParseM s :: Maybe Day
  ]

-- ORDER BY extraction ---------------------------------------------------------

-- SortBy = UsingSortBy AExpr QualAllOp (Maybe NullsOrder)
--        | AscDescSortBy AExpr (Maybe AscDesc) (Maybe NullsOrder)
extractSort :: SortBy -> Either QueryError SortSpec
extractSort (AscDescSortBy expr mAscDesc _) = do
  col <- extractAExprColName expr
  let dir = case mAscDesc of
              Just DescAscDesc -> Desc
              _                -> Asc
  pure (SortSpec col dir)
extractSort s =
  Left (UnsupportedSyntax ("unsupported ORDER BY expression: " <> T.pack (show s)))

-- LIMIT extraction ------------------------------------------------------------

extractSelectLimit :: SelectLimit -> Either QueryError Int
extractSelectLimit (LimitSelectLimit lc)          = extractLimitClause lc
extractSelectLimit (LimitOffsetSelectLimit lc _)  = extractLimitClause lc
extractSelectLimit (OffsetLimitSelectLimit _ lc)  = extractLimitClause lc
extractSelectLimit (OffsetSelectLimit _)          =
  Left (UnsupportedSyntax "OFFSET without LIMIT not supported")

extractLimitClause :: LimitClause -> Either QueryError Int
extractLimitClause (LimitLimitClause (ExprSelectLimitValue expr) _) = extractLimitExpr expr
extractLimitClause (LimitLimitClause AllSelectLimitValue _) =
  Left (UnsupportedSyntax "LIMIT ALL not supported")
extractLimitClause (FetchOnlyLimitClause _ _ _) =
  Left (UnsupportedSyntax "FETCH FIRST/NEXT not supported")

extractLimitExpr :: AExpr -> Either QueryError Int
extractLimitExpr (CExprAExpr (AexprConstCExpr (IAexprConst i))) = Right (fromIntegral i)
extractLimitExpr expr =
  Left (UnsupportedSyntax ("unsupported LIMIT expression: " <> T.pack (show expr)))

-- GROUP BY extraction ---------------------------------------------------------

extractGroupCol :: GroupByItem -> Either QueryError ColumnName
extractGroupCol (ExprGroupByItem expr) = extractAExprColName expr
extractGroupCol g =
  Left (UnsupportedSyntax ("unsupported GROUP BY expression: " <> T.pack (show g)))
