{-# LANGUAGE QuasiQuotes #-}

module Model.Tag (
  list,
) where

import Database.SQLite.Simple (Query, fromOnly, queryNamed)
import Database.SQLite.Simple.QQ (sql)
import Model.Common (DBAction)
import Relude hiding (on)

list :: DBAction [Text]
list = do
  conn <- ask
  let q :: Query
      q = [sql| SELECT tag.name FROM tag JOIN article_tag ON article_tag.tagid = tag.id |]
  results <- liftIO $ queryNamed conn q []
  pure $ fromOnly <$> results
