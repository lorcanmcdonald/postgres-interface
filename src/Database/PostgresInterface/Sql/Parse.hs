module Database.PostgresInterface.Sql.Parse
  ( parseSql
  , SqlAst
  ) where

import Data.Text (Text)
import Language.SQL.SimpleSQL.Syntax (QueryExpr)

-- | Re-export the SimpleSQL AST type under our own alias
type SqlAst = QueryExpr

-- | Parse a SQL string into an AST
parseSql :: Text -> Either String SqlAst
parseSql = error "not yet implemented"
