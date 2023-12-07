module Model.Common where

import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.Aeson as Aeson
import Data.Char (isLower, isUpper, toLower)
import qualified Data.OpenApi as OpenApi
import Data.Pool (Pool, withResource)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (Update)
import Database.Persist.Sqlite (withSqlitePool)
import Model.Entities (migrateAll)
import Relude

-- All DB operations run in this monad
type DBAction a = ReaderT SqlBackend IO a

withDBConnectionPool :: (Pool SqlBackend -> IO a) -> IO a
withDBConnectionPool f = runStdoutLoggingT
  $ withSqlitePool "realworld.db" 20
  $ \pool -> liftIO $ do
    withResource pool $ runSqlConn (runMigration migrateAll)
    f pool

-- An optional update operator
(=?.) :: (PersistEntity v, PersistField typ) => EntityField v typ -> Maybe typ -> Maybe (SqlExpr (Entity v) -> SqlExpr Update)
fld =?. mv = fmap (\v -> fld =. val v) mv

-- Aeson options to drop the entity name prefix from field names
aesonDropPrefixOptions :: Aeson.Options
aesonDropPrefixOptions = Aeson.defaultOptions{Aeson.fieldLabelModifier = lowerFirst . dropWhile isLower}

schemaDropPrefixOptions :: OpenApi.SchemaOptions
schemaDropPrefixOptions = OpenApi.defaultSchemaOptions{OpenApi.fieldLabelModifier = lowerFirst . dropWhile isLower}

lowerFirst :: String -> String
lowerFirst (c : cs) | isUpper c = toLower c : cs
lowerFirst s = s
