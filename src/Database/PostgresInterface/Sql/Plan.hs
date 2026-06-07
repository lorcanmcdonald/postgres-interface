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

import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Foldable (asum)
import Data.Time (Day, LocalTime (..), UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Database.PostgresInterface.Sql.Parse (SqlAst)
import Language.SQL.SimpleSQL.Syntax hiding (Asc, Desc, SortSpec, Statement)
import Language.SQL.SimpleSQL.Syntax qualified as SSS

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
  | Like      ColumnName Text              -- ^ col LIKE pattern  (% and _ wildcards)
  | ILike     ColumnName Text              -- ^ col ILIKE pattern (case-insensitive)
  | IsNull    ColumnName
  | IsNotNull ColumnName
  | ColIn     ColumnName [ScalarLiteral]   -- ^ col IN (...)
  | ColNotIn  ColumnName [ScalarLiteral]   -- ^ col NOT IN (...)
  | Not       Predicate
  | And       Predicate Predicate
  | Or        Predicate Predicate
  deriving (Eq, Show)

data QueryPlan = QueryPlan
  { qpFrom      :: [TableSource]                   -- ^ table sources (cross joined for multiple)
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
toQueryPlan (SSS.SelectStatement qe) = toQueryPlanSelect qe
toQueryPlan (SSS.Insert {})          = Left (UnsupportedOperation "INSERT is not supported")
toQueryPlan (SSS.Update {})          = Left (UnsupportedOperation "UPDATE is not supported")
toQueryPlan (SSS.Delete {})          = Left (UnsupportedOperation "DELETE is not supported")
toQueryPlan SSS.EmptyStatement       = Left (UnsupportedSyntax "empty statement")
toQueryPlan _                        = Left (UnsupportedSyntax "only SELECT statements are supported")

toQueryPlanSelect :: QueryExpr -> Either QueryError QueryPlan
toQueryPlanSelect (Select _ selectList from mWhere groupBy _ orderBy _ mLimit) = do
  (srcs, fromPreds) <- extractFrom from
  (cols, aliases)   <- extractColumns selectList
  wherePreds        <- maybe (Right []) (\w -> (:[]) <$> extractPredicate w) mWhere
  sorts             <- mapM extractSort orderBy
  limit             <- mapM extractLimit mLimit
  groups            <- mapM extractGroupCol groupBy
  pure QueryPlan
    { qpFrom       = srcs
    , qpColumns    = cols
    , qpAliases    = aliases
    , qpPredicates = fromPreds ++ wherePreds
    , qpOrderBy    = sorts
    , qpLimit      = limit
    , qpGroupBy    = groups
    }
toQueryPlanSelect _ =
  Left (UnsupportedSyntax "only SELECT statements are supported")

-- FROM extraction -------------------------------------------------------------

extractFrom :: [TableRef] -> Either QueryError ([TableSource], [Predicate])
extractFrom [] = Left (UnsupportedSyntax "no FROM clause")
extractFrom refs = do
  pairs <- mapM extractFromRef refs
  pure (concatMap fst pairs, concatMap snd pairs)

extractFromRef :: TableRef -> Either QueryError ([TableSource], [Predicate])
extractFromRef (TRSimple names) =
  Right ([TableRef (namesToText names) Nothing], [])
extractFromRef (TRAlias (TRSimple names) (Alias (Name _ alias) _)) =
  Right ([TableRef (namesToText names) (Just alias)], [])
extractFromRef (TRAlias (TRQueryExpr qe) (Alias (Name _ alias) _)) = do
  innerPlan <- toQueryPlanSelect qe
  pure ([Subquery innerPlan alias], [])
extractFromRef (TRJoin lhs _ _ rhs cond) = do
  (lhsSrcs, lhsPreds) <- extractFromRef lhs
  (rhsSrcs, rhsPreds) <- extractFromRef rhs
  condPreds <- case cond of
    Nothing             -> Right []
    Just (JoinOn e)     -> (:[]) <$> extractPredicate e
    Just (JoinUsing _)  -> Left (UnsupportedSyntax "JOIN USING not supported")
  pure (lhsSrcs ++ rhsSrcs, lhsPreds ++ rhsPreds ++ condPreds)
extractFromRef (TRParens ref) = extractFromRef ref
extractFromRef ref =
  Left (UnsupportedSyntax ("unsupported FROM clause: " <> T.pack (show ref)))

namesToText :: [Name] -> Text
namesToText names = T.intercalate "." [n | Name _ n <- names]

-- Column extraction -----------------------------------------------------------

-- | Returns the column selection and a list of (original, alias) pairs for
-- columns that have an explicit alias different from their source name.
extractColumns :: [(ScalarExpr, Maybe Name)] -> Either QueryError (ColumnSelection, [(ColumnName, ColumnName)])
extractColumns [(Star, _)] = Right (AllColumns, [])
extractColumns cols = do
  pairs <- mapM extractColWithAlias cols
  let names   = map fst pairs
      aliases = [(orig, alias) | (orig, alias) <- pairs, orig /= alias]
  pure (NamedColumns names, aliases)

-- | Returns (original column name, output name) — output name is the alias
-- when one is present, otherwise the original name.
extractColWithAlias :: (ScalarExpr, Maybe Name) -> Either QueryError (ColumnName, ColumnName)
extractColWithAlias (Iden names, Just (Name _ alias)) =
  Right (namesToText names, alias)
extractColWithAlias (Iden names, Nothing) =
  let n = namesToText names in Right (n, n)
extractColWithAlias (_, Just (Name _ alias)) =
  Right (alias, alias)
extractColWithAlias (expr, _) =
  Left (UnsupportedSyntax ("unsupported column expression: " <> T.pack (show expr)))

-- Predicate extraction --------------------------------------------------------

extractPredicate :: ScalarExpr -> Either QueryError Predicate
extractPredicate (BinOp lhs [Name _ "and"] rhs) =
  And <$> extractPredicate lhs <*> extractPredicate rhs
extractPredicate (BinOp lhs [Name _ "or"] rhs) =
  Or <$> extractPredicate lhs <*> extractPredicate rhs
-- col LIKE / ILIKE — before the generic BinOp case to avoid shadowing
extractPredicate (BinOp (Iden names) [Name _ "like"] (StringLit _ _ pat)) =
  Right (Like (namesToText names) pat)
extractPredicate (BinOp (Iden names) [Name _ "ilike"] (StringLit _ _ pat)) =
  Right (ILike (namesToText names) pat)
-- col op col  (e.g. JOIN ON t.id = u.id) — before col op literal
extractPredicate (BinOp (Iden lhsNames) [Name _ op] (Iden rhsNames)) = do
  compOp <- parseCompOp op
  pure (ColCmpCol (namesToText lhsNames) compOp (namesToText rhsNames))
-- col op literal
extractPredicate (BinOp (Iden names) [Name _ op] rhs) = do
  col     <- Right (namesToText names)
  compOp  <- parseCompOp op
  literal <- extractLiteral rhs
  pure (ColOp col compOp literal)
extractPredicate (Parens e) = extractPredicate e
extractPredicate (SpecialOp [Name _ "between"] [Iden names, lo, hi]) = do
  col   <- Right (namesToText names)
  loLit <- extractLiteral lo
  hiLit <- extractLiteral hi
  pure (And (ColOp col Ge loLit) (ColOp col Le hiLit))
extractPredicate (PostfixOp [Name _ "is null"] (Iden names)) =
  Right (IsNull (namesToText names))
extractPredicate (PostfixOp [Name _ "is not null"] (Iden names)) =
  Right (IsNotNull (namesToText names))
extractPredicate (In False (Iden names) (InList exprs)) = do
  lits <- mapM extractLiteralStrict exprs
  pure (ColIn (namesToText names) lits)
extractPredicate (In True (Iden names) (InList exprs)) = do
  lits <- mapM extractLiteralStrict exprs
  pure (ColNotIn (namesToText names) lits)
extractPredicate (PrefixOp [Name _ "not"] e) =
  Not <$> extractPredicate e
extractPredicate expr =
  Left (UnsupportedSyntax ("unsupported predicate: " <> T.pack (show expr)))

parseCompOp :: Text -> Either QueryError CompOp
parseCompOp "="  = Right Eq
parseCompOp "<>" = Right Ne
parseCompOp "!=" = Right Ne
parseCompOp "<"  = Right Lt
parseCompOp "<=" = Right Le
parseCompOp ">"  = Right Gt
parseCompOp ">=" = Right Ge
parseCompOp op   = Left (UnsupportedSyntax ("unsupported operator: " <> op))

extractLiteral :: ScalarExpr -> Either QueryError ScalarLiteral
extractLiteral (StringLit _ _ val) =
  case parseIsoDate (T.unpack val) of
    Just d  -> Right (LitDate d)
    Nothing -> Right (LitText val)
extractLiteral (NumLit n) =
  case reads (T.unpack n) of
    [(i, "")] -> Right (LitNum (fromIntegral (i :: Int)))
    _         -> Left (UnsupportedSyntax ("cannot parse numeric literal: " <> n))
extractLiteral expr =
  Left (UnsupportedSyntax ("unsupported literal: " <> T.pack (show expr)))

-- | Like 'extractLiteral' but treats all string literals as text (no ISO date
-- inference). Used for IN-list values where the user is comparing against text.
extractLiteralStrict :: ScalarExpr -> Either QueryError ScalarLiteral
extractLiteralStrict (StringLit _ _ val) = Right (LitText val)
extractLiteralStrict expr                = extractLiteral expr

-- | Try to extract a 'Day' from an ISO 8601 string, accepting any valid
-- combination of date, local datetime, or UTC datetime.
parseIsoDate :: String -> Maybe Day
parseIsoDate s = asum
  [ utctDay  <$> (iso8601ParseM s :: Maybe UTCTime)
  , localDay <$> (iso8601ParseM s :: Maybe LocalTime)
  ,               iso8601ParseM s :: Maybe Day
  ]

-- ORDER BY extraction ---------------------------------------------------------

extractSort :: SSS.SortSpec -> Either QueryError SortSpec
extractSort (SSS.SortSpec (Iden names) dir _) =
  Right (SortSpec (namesToText names) (convertDir dir))
extractSort s =
  Left (UnsupportedSyntax ("unsupported ORDER BY expression: " <> T.pack (show s)))

convertDir :: Direction -> SortDir
convertDir SSS.Asc        = Asc
convertDir SSS.DirDefault = Asc
convertDir SSS.Desc       = Desc

-- LIMIT extraction ------------------------------------------------------------

extractLimit :: ScalarExpr -> Either QueryError Int
extractLimit (NumLit n) =
  case reads (T.unpack n) of
    [(i, "")] -> Right i
    _         -> Left (UnsupportedSyntax ("cannot parse LIMIT: " <> n))
extractLimit expr =
  Left (UnsupportedSyntax ("unsupported LIMIT expression: " <> T.pack (show expr)))

-- GROUP BY extraction ---------------------------------------------------------

extractGroupCol :: GroupingExpr -> Either QueryError ColumnName
extractGroupCol (SimpleGroup (Iden names)) = Right (namesToText names)
extractGroupCol g =
  Left (UnsupportedSyntax ("unsupported GROUP BY expression: " <> T.pack (show g)))
