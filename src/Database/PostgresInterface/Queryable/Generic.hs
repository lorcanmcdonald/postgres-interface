-- | Re-export of generic deriving support.
-- Import this module (even with an empty import list) to bring the
-- DefaultSignatures machinery into scope for your Queryable instances.
--
-- Example:
--   import Database.PostgresInterface.Queryable (Queryable)
--   import Database.PostgresInterface.Queryable.Generic ()
--
--   data MyType = MyType { myField :: Text, myCount :: Int32 }
--     deriving Generic
--
--   instance Queryable MyType  -- schema and toRow derived automatically

module Database.PostgresInterface.Queryable.Generic () where
