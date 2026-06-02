{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example postgres-interface server.
--
-- Exposes two tables:
--   employees  – a static list of employees
--   metrics    – a time-series of daily CPU samples
--
-- Run with:
--   cabal run example
--
-- Then connect with psql:
--   psql -h 127.0.0.1 -p 5433 -U anyuser anydb
--
-- Try:
--   SELECT * FROM employees;
--   SELECT * FROM employees WHERE department = 'Engineering';
--   SELECT * FROM metrics WHERE sample_date >= '2024-01-03';
--   SELECT * FROM metrics ORDER BY cpu_pct DESC LIMIT 3;

module Main (main) where

import Conduit (yieldMany)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

import Database.PostgresInterface.Queryable
import Database.PostgresInterface.Queryable.Generic ()
import Database.PostgresInterface.Serve (ServerConfig (..), servePostgres)
import Database.PostgresInterface.Table (naiveTable)

-- ---------------------------------------------------------------------------
-- employees table
-- ---------------------------------------------------------------------------

data Employee = Employee
  { name       :: Text
  , department :: Text
  , salary     :: Int32
  } deriving (Generic, Show)

instance Queryable Employee

employees :: [Employee]
employees =
  [ Employee "Alice"   "Engineering" 95000
  , Employee "Bob"     "Engineering" 88000
  , Employee "Carol"   "Marketing"   72000
  , Employee "Dave"    "Marketing"   68000
  , Employee "Eve"     "Operations"  61000
  ]

-- ---------------------------------------------------------------------------
-- metrics table
-- ---------------------------------------------------------------------------

data Metric = Metric
  { sample_date :: Day
  , host        :: Text
  , cpu_pct     :: Int32
  } deriving (Generic, Show)

instance Queryable Metric

metrics :: [Metric]
metrics =
  [ Metric (fromGregorian 2024 1 1) "web-01" 42
  , Metric (fromGregorian 2024 1 2) "web-01" 55
  , Metric (fromGregorian 2024 1 3) "web-01" 71
  , Metric (fromGregorian 2024 1 4) "web-01" 38
  , Metric (fromGregorian 2024 1 1) "web-02" 60
  , Metric (fromGregorian 2024 1 2) "web-02" 80
  , Metric (fromGregorian 2024 1 3) "web-02" 45
  , Metric (fromGregorian 2024 1 4) "web-02" 52
  ]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Listening on 127.0.0.1:5433"
  putStrLn "Connect: psql -h 127.0.0.1 -p 5433 -U anyuser anydb"
  servePostgres cfg
    [ naiveTable "employees" (pure (yieldMany employees))
    , naiveTable "metrics"   (pure (yieldMany metrics))
    ]
  where
    cfg = ServerConfig { serverPort = 5433, serverHost = Just "127.0.0.1" }
