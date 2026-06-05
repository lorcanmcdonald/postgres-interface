{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.Plan
  ( QueryPlan (..)
  , ColumnSelection (..)
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
import Data.Time (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Database.PostgresInterface.Sql.Parse (SqlAst)
import Language.SQL.SimpleSQL.Syntax hiding (Asc, Desc, SortSpec)
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

data Predicate
  = ColOp ColumnName CompOp ScalarLiteral
  | And   Predicate  Predicate
  | Or    Predicate  Predicate
  deriving (Eq, Show)

data QueryPlan = QueryPlan
  { qpTable      :: ColumnName
  , qpColumns    :: ColumnSelection
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
toQueryPlan (Select _ selectList from mWhere groupBy _ orderBy _ mLimit) = do
  tableName <- extractTable from
  cols      <- extractColumns selectList
  preds     <- maybe (Right []) (\w -> (:[]) <$> extractPredicate w) mWhere
  sorts     <- mapM extractSort orderBy
  limit     <- mapM extractLimit mLimit
  groups    <- mapM extractGroupCol groupBy
  pure QueryPlan
    { qpTable      = tableName
    , qpColumns    = cols
    , qpPredicates = preds
    , qpOrderBy    = sorts
    , qpLimit      = limit
    , qpGroupBy    = groups
    }
toQueryPlan _ =
  Left (UnsupportedSyntax "only SELECT statements are supported")

-- Table extraction ------------------------------------------------------------

extractTable :: [TableRef] -> Either QueryError ColumnName
extractTable [TRSimple names] = Right (namesToText names)
extractTable [TRAlias (TRSimple names) _] = Right (namesToText names)
extractTable []  = Left (UnsupportedSyntax "no FROM clause")
extractTable [_] = Left (UnsupportedSyntax "unsupported FROM clause (JOINs, subqueries not supported)")
extractTable _   = Left (UnsupportedSyntax "multiple tables in FROM not supported")

namesToText :: [Name] -> Text
namesToText names = T.intercalate "." [n | Name _ n <- names]

-- Column extraction -----------------------------------------------------------

extractColumns :: [(ScalarExpr, Maybe Name)] -> Either QueryError ColumnSelection
extractColumns [(Star, _)] = Right AllColumns
extractColumns cols = NamedColumns <$> mapM extractColName cols

extractColName :: (ScalarExpr, Maybe Name) -> Either QueryError ColumnName
extractColName (Iden names, _) = Right (namesToText names)
extractColName (_, Just (Name _ alias)) = Right alias
extractColName (expr, _) = Left (UnsupportedSyntax ("unsupported column expression: " <> T.pack (show expr)))

-- Predicate extraction --------------------------------------------------------

extractPredicate :: ScalarExpr -> Either QueryError Predicate
extractPredicate (BinOp lhs [Name _ "and"] rhs) =
  And <$> extractPredicate lhs <*> extractPredicate rhs
extractPredicate (BinOp lhs [Name _ "or"] rhs) =
  Or <$> extractPredicate lhs <*> extractPredicate rhs
extractPredicate (BinOp (Iden names) [Name _ op] rhs) = do
  col     <- Right (namesToText names)
  compOp  <- parseCompOp op
  literal <- extractLiteral rhs
  pure (ColOp col compOp literal)
extractPredicate (Parens e) = extractPredicate e
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
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack val) of
    Just d  -> Right (LitDate d)
    Nothing -> Right (LitText val)
extractLiteral (NumLit n) =
  case reads (T.unpack n) of
    [(i, "")] -> Right (LitNum (fromIntegral (i :: Int)))
    _         -> Left (UnsupportedSyntax ("cannot parse numeric literal: " <> n))
extractLiteral expr =
  Left (UnsupportedSyntax ("unsupported literal: " <> T.pack (show expr)))

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
