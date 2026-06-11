module Main (main) where

import Test.Tasty
import Protocol.MessagesSpec qualified
import Protocol.IntegrationSpec qualified
import Sql.PlanSpec qualified
import Sql.SelectSupportSpec qualified
import QueryableSpec qualified
import EndToEndSpec qualified
import TableSpec qualified
import K8sSpec qualified

main :: IO ()
main = defaultMain $ testGroup "postgres-interface"
  [ Protocol.MessagesSpec.tests
  , Protocol.IntegrationSpec.tests
  , Sql.PlanSpec.tests
  , Sql.SelectSupportSpec.tests
  , QueryableSpec.tests
  , EndToEndSpec.tests
  , TableSpec.tests
  , K8sSpec.tests
  ]
