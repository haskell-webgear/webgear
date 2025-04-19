{-# LANGUAGE QuasiQuotes #-}

module Model.Tag (
  list,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import Database.SQLite.Simple (Query, fromOnly, queryNamed)
import Database.SQLite.Simple.QQ (sql)
import Model.Common (DBAction)

list :: DBAction [Text]
list = do
  conn <- ask
  let q :: Query
      q = [sql| SELECT tag.name FROM tag JOIN article_tag ON article_tag.tagid = tag.id |]
  results <- liftIO $ queryNamed conn q []
  pure $ fromOnly <$> results
