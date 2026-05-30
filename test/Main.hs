module Main (main) where

import Test.Tasty
import Protocol.MessagesSpec qualified
import Protocol.IntegrationSpec qualified
import Sql.PlanSpec qualified
import QueryableSpec qualified
import EndToEndSpec qualified

main :: IO ()
main = defaultMain $ testGroup "postgres-interface"
  [ Protocol.MessagesSpec.tests
  , Protocol.IntegrationSpec.tests
  , Sql.PlanSpec.tests
  , QueryableSpec.tests
  , EndToEndSpec.tests
  ]
