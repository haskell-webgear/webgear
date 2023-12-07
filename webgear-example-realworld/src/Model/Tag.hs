module Model.Tag (
  list,
) where

import Database.Esqueleto.Experimental
import Model.Common (DBAction)
import Model.Entities
import Relude hiding (on)

list :: DBAction [Text]
list =
  fmap unValue
    <$>
    -- select only tags used in articles
    ( select $ do
        (tag :& _articleTag) <-
          from $
            table @Tag
              `innerJoin` table @ArticleTag
              `on` (\(tag :& articleTag) -> tag ^. TagId ==. articleTag ^. ArticleTagTagid)
        pure $ tag ^. TagName
    )
