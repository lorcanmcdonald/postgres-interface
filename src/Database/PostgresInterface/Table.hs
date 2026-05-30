module Database.PostgresInterface.Table
  ( Table (..)
  , AnyTable
  , anyTable
  , naiveTable
  ) where

import Conduit (ConduitT)
import Database.PostgresInterface.Queryable (Queryable)
import Database.PostgresInterface.Sql.Plan (QueryPlan)

data Table a = Table
  { tableName    :: String
  , tableHandler :: QueryPlan -> IO (ConduitT () a IO ())
  }

data AnyTable = forall a. Queryable a => AnyTable (Table a)

-- | Smart constructor: wrap a typed table handler into AnyTable
anyTable
  :: Queryable a
  => String
  -> (QueryPlan -> IO (ConduitT () a IO ()))
  -> AnyTable
anyTable name handler = AnyTable (Table name handler)

-- | Easy-path: provide a plain stream; library applies predicates
naiveTable
  :: Queryable a
  => String
  -> IO (ConduitT () a IO ())
  -> AnyTable
naiveTable name stream = anyTable name (\_plan -> stream)
