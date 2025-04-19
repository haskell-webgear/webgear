module Model.Common where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT)
import qualified Data.Aeson as Aeson
import Data.Char (isLower, isUpper, toLower)
import qualified Data.OpenApi as OpenApi
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Database.SQLite.Simple (
  Connection,
  FromRow,
  NamedParam,
  Query,
  close,
  executeNamed,
  open,
  queryNamed,
 )
import Model.Entities (migrateAll)

-- All DB operations run in this monad
type DBAction a = ReaderT Connection IO a

withDBConnectionPool :: (Pool Connection -> IO a) -> IO a
withDBConnectionPool f = do
  pool <- newPool $ defaultPoolConfig (open "realworld.db") close 300.0 20
  withResource pool migrateAll
  f pool

queryNamed :: (FromRow r) => Query -> [NamedParam] -> DBAction [r]
queryNamed q params = do
  conn <- ask
  liftIO $ Database.SQLite.Simple.queryNamed conn q params

executeNamed :: Query -> [NamedParam] -> DBAction ()
executeNamed q params = do
  conn <- ask
  liftIO $ Database.SQLite.Simple.executeNamed conn q params

-- Aeson options to drop the entity name prefix from field names
aesonDropPrefixOptions :: Aeson.Options
aesonDropPrefixOptions = Aeson.defaultOptions{Aeson.fieldLabelModifier = lowerFirst . dropWhile isLower}

schemaDropPrefixOptions :: OpenApi.SchemaOptions
schemaDropPrefixOptions = OpenApi.defaultSchemaOptions{OpenApi.fieldLabelModifier = lowerFirst . dropWhile isLower}

lowerFirst :: String -> String
lowerFirst (c : cs) | isUpper c = toLower c : cs
lowerFirst s = s
