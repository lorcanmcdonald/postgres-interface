{-# LANGUAGE OverloadedStrings #-}

module Database.PostgresInterface.Sql.PredicateEval
  ( EvalError (..)
  , Row
  , evalPredicate
  , evalPredicates
  , compareColumnValues
  , lookupCI
  , lookupCol
  ) where

import Control.Exception (Exception)
import Data.List (find, tails)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgresInterface.Queryable (ColumnName, ColumnValue (..))
import Database.PostgresInterface.Sql.Plan
  ( CompOp (..)
  , Predicate (..)
  , ScalarLiteral (..)
  )

-- | A row in the flat association-list representation used during evaluation.
-- Columns appear both unqualified ("col") and qualified ("table.col") so that
-- simple WHERE predicates and qualified JOIN ON predicates both resolve.
type Row = [(Text, ColumnValue)]

data EvalError
  = ColumnNotFound ColumnName
  | TypeMismatch   ColumnValue ScalarLiteral
  deriving (Eq, Show)

instance Exception EvalError

-- | Evaluate a single predicate against a row.
-- Returns @Left (ColumnNotFound col)@ when a referenced column is absent from
-- the row, and @Left (TypeMismatch val lit)@ when the column value type is
-- incompatible with the literal in the predicate.
evalPredicate :: Row -> Predicate -> Either EvalError Bool
evalPredicate vals (And l r)  = (&&) <$> evalPredicate vals l <*> evalPredicate vals r
evalPredicate vals (Or  l r)  = (||) <$> evalPredicate vals l <*> evalPredicate vals r
evalPredicate vals (Not p)    = not  <$> evalPredicate vals p
evalPredicate vals (ColOp col op lit) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> evalCompOp op val lit
evalPredicate vals (ColCmpCol col1 op col2) =
  case (lookupCol col1 vals, lookupCol col2 vals) of
    (Nothing, _)       -> Left (ColumnNotFound col1)
    (_, Nothing)       -> Left (ColumnNotFound col2)
    (Just v1, Just v2) -> Right (evalColCmp op v1 v2)
evalPredicate vals (Like col pat) =
  case lookupCol col vals of
    Nothing            -> Left (ColumnNotFound col)
    Just (PgTextVal t) -> Right (matchLike pat t)
    Just val           -> Left (TypeMismatch val (LitText pat))
evalPredicate vals (ILike col pat) =
  case lookupCol col vals of
    Nothing            -> Left (ColumnNotFound col)
    Just (PgTextVal t) -> Right (matchLike (T.toLower pat) (T.toLower t))
    Just val           -> Left (TypeMismatch val (LitText pat))
evalPredicate vals (IsTrue col) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> Right (val == PgBoolVal True)
evalPredicate vals (IsFalse col) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> Right (val == PgBoolVal False)
evalPredicate vals (IsNull col) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> Right (val == PgNull)
evalPredicate vals (IsNotNull col) =
  case lookupCol col vals of
    Nothing     -> Left (ColumnNotFound col)
    Just PgNull -> Right False
    Just _      -> Right True
evalPredicate vals (ColIn col lits) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> fmap or (traverse (evalCompOp Eq val) lits)
evalPredicate vals (ColNotIn col lits) =
  case lookupCol col vals of
    Nothing  -> Left (ColumnNotFound col)
    Just val -> fmap (all not) (traverse (evalCompOp Eq val) lits)

-- | Evaluate all predicates in a list against a row; short-circuits on the
-- first error.
evalPredicates :: Row -> [Predicate] -> Either EvalError Bool
evalPredicates vals preds = fmap and (traverse (evalPredicate vals) preds)

evalCompOp :: CompOp -> ColumnValue -> ScalarLiteral -> Either EvalError Bool
evalCompOp op (PgInt4Val a)    (LitInt b)  = Right (applyOp op a (fromIntegral b))
evalCompOp op (PgInt8Val a)    (LitInt b)  = Right (applyOp op a (fromIntegral b))
evalCompOp op (PgInt4Val a)    (LitNum b)  = Right (applyOp op (toScientific a) b)
evalCompOp op (PgInt8Val a)    (LitNum b)  = Right (applyOp op (toScientific a) b)
evalCompOp op (PgNumericVal a) (LitNum b)  = Right (applyOp op a b)
evalCompOp op (PgTextVal a)    (LitText b) = Right (applyOp op a b)
evalCompOp op (PgDateVal a)    (LitDate b) = Right (applyOp op a b)
evalCompOp _  val              lit         = Left (TypeMismatch val lit)

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

-- | Compare two nullable ColumnValues for ordering. NULLs sort before
-- non-NULLs; mismatched types compare as EQ (no meaningful ordering).
compareColumnValues :: Maybe ColumnValue -> Maybe ColumnValue -> Ordering
compareColumnValues (Just (PgInt4Val a))    (Just (PgInt4Val b))    = compare a b
compareColumnValues (Just (PgInt8Val a))    (Just (PgInt8Val b))    = compare a b
compareColumnValues (Just (PgTextVal a))    (Just (PgTextVal b))    = compare a b
compareColumnValues (Just (PgDateVal a))    (Just (PgDateVal b))    = compare a b
compareColumnValues (Just (PgBoolVal a))    (Just (PgBoolVal b))    = compare a b
compareColumnValues Nothing                 Nothing                  = EQ
compareColumnValues Nothing                 _                        = LT
compareColumnValues _                       Nothing                  = GT
compareColumnValues _                       _                        = EQ

-- | Naive SQL LIKE pattern match. '%' matches any sequence of characters,
-- '_' matches exactly one character; all other characters match literally.
matchLike :: Text -> Text -> Bool
matchLike pat str = go (T.unpack pat) (T.unpack str)
  where
    go []         []    = True
    go ('%' : ps) s     = any (go ps) (tails s)
    go ('_' : ps) (_:s) = go ps s
    go (p   : ps) (c:s) = p == c && go ps s
    go _          _     = False

-- | Case-insensitive lookup in an association list.
lookupCI :: Text -> [(Text, a)] -> Maybe a
lookupCI col = fmap snd . find (\(k, _) -> T.toLower k == T.toLower col)

-- | Look up a column value by name (case-insensitive). When the name is
-- unqualified and absent, falls back to a unique suffix match (e.g. "id"
-- matches "t.id") so simple predicates work against cross-product rows.
lookupCol :: Text -> Row -> Maybe ColumnValue
lookupCol col vals =
  case lookupCI col vals of
    Just v  -> Just v
    Nothing ->
      let suffix  = "." <> T.toLower col
          matches = [v | (k, v) <- vals, T.isSuffixOf suffix (T.toLower k)]
      in case matches of
           [v] -> Just v
           _   -> Nothing

toScientific :: Integral a => a -> Scientific
toScientific = fromIntegral

applyOp :: Ord b => CompOp -> b -> b -> Bool
applyOp Eq a b = a == b
applyOp Ne a b = a /= b
applyOp Lt a b = a <  b
applyOp Le a b = a <= b
applyOp Gt a b = a >  b
applyOp Ge a b = a >= b
