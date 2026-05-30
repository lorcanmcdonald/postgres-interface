-- | Predicate inspection helpers for QueryPlan.
-- These functions allow table handlers to extract typed bounds from the
-- WHERE clause so they can push filtering down to the data source.
module Database.PostgresInterface.Table.Pushdown
  ( dateRange
  , intRange
  ) where

import Data.Text (Text)
import Data.Time (Day, addDays, fromGregorian)
import Database.PostgresInterface.Sql.Plan

-- | Extract a date range for a named column from all predicates in the plan.
-- Returns @Just (lo, hi)@ if any relevant predicate exists, @Nothing@ otherwise.
-- Strict inequalities (@<@, @>@) are converted to inclusive bounds by adding/subtracting one day.
dateRange :: QueryPlan -> Text -> Maybe (Day, Day)
dateRange plan col =
  let bounds = concatMap (collectDateBounds col) (qpPredicates plan)
  in if null bounds
       then Nothing
       else Just (foldr1 intersectDate bounds)

intersectDate :: (Day, Day) -> (Day, Day) -> (Day, Day)
intersectDate (lo1, hi1) (lo2, hi2) = (max lo1 lo2, min hi1 hi2)

collectDateBounds :: Text -> Predicate -> [(Day, Day)]
collectDateBounds col (And l r) = collectDateBounds col l <> collectDateBounds col r
collectDateBounds col (Or  l r) = collectDateBounds col l <> collectDateBounds col r
collectDateBounds col (ColOp c op (LitDate d))
  | c == col  = maybe [] (:[]) (dateBound op d)
  | otherwise = []
collectDateBounds _ _ = []

-- Practical sentinels: year 0001-01-01 .. 9999-12-31
minDay :: Day
minDay = fromGregorian 1 1 1

maxDay :: Day
maxDay = fromGregorian 9999 12 31

dateBound :: CompOp -> Day -> Maybe (Day, Day)
dateBound Ge d = Just (d,           maxDay)
dateBound Gt d = Just (addDays 1 d, maxDay)
dateBound Le d = Just (minDay,      d)
dateBound Lt d = Just (minDay,      addDays (-1) d)
dateBound _  _ = Nothing

-- | Extract an integer range for a named column from all predicates in the plan.
intRange :: QueryPlan -> Text -> Maybe (Int, Int)
intRange plan col =
  let bounds = concatMap (collectIntBounds col) (qpPredicates plan)
  in if null bounds
       then Nothing
       else Just (foldr1 intersectInt bounds)

intersectInt :: (Int, Int) -> (Int, Int) -> (Int, Int)
intersectInt (lo1, hi1) (lo2, hi2) = (max lo1 lo2, min hi1 hi2)

collectIntBounds :: Text -> Predicate -> [(Int, Int)]
collectIntBounds col (And l r) = collectIntBounds col l <> collectIntBounds col r
collectIntBounds col (Or  l r) = collectIntBounds col l <> collectIntBounds col r
collectIntBounds col (ColOp c op (LitInt n))
  | c == col  = maybe [] (:[]) (intBound op n)
  | otherwise = []
collectIntBounds _ _ = []

intBound :: CompOp -> Int -> Maybe (Int, Int)
intBound Ge n = Just (n,     maxBound)
intBound Gt n = Just (n + 1, maxBound)
intBound Le n = Just (minBound, n)
intBound Lt n = Just (minBound, n - 1)
intBound _  _ = Nothing
