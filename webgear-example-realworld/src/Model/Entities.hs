{-# LANGUAGE QuasiQuotes #-}

module Model.Entities where

import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection, Query, execute_)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.FromRow (FromRow)
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField)
import Database.SQLite.Simple.ToRow (ToRow)
import Relude

data User = User
  { userId :: !UserId
  , userUsername :: !Text
  , userEmail :: !Text
  , userPassword :: !Text
  , userBio :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

newtype UserId = UserId Int64
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Num, FromField, ToField)

data Article = Article
  { articleId :: !ArticleId
  , articleSlug :: !Text
  , articleTitle :: !Text
  , articleDescription :: !Text
  , articleBody :: !Text
  , articleCreatedAt :: !UTCTime
  , articleUpdatedAt :: !UTCTime
  , articleAuthor :: !UserId
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

newtype ArticleId = ArticleId Int64
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Num, FromField, ToField)

data Tag = Tag
  { tagId :: !TagId
  , name :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

newtype TagId = TagId Int64
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Num, FromField, ToField)

data ArticleTag = ArticleTag
  { tagid :: !TagId
  , articleId :: !ArticleId
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

data Comment = Comment
  { commentId :: !CommentId
  , createdAt :: !UTCTime
  , updatedAt :: !UTCTime
  , body :: !Text
  , article :: !ArticleId
  , author :: !UserId
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

newtype CommentId = CommentId Int64
  deriving stock (Show, Read)
  deriving newtype (Eq, Ord, Num, FromField, ToField)

data Favorite = Favorite
  { userid :: !UserId
  , articleId :: !ArticleId
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

data Follow = Follow
  { follower :: !UserId
  , followee :: !UserId
  }
  deriving stock (Generic)
  deriving anyclass (FromRow, ToRow)

migrateAll :: Connection -> IO ()
migrateAll conn = do
  let ddls :: [Query]
      ddls =
        [ [sql|
            CREATE TABLE IF NOT EXISTS user (
              id INTEGER PRIMARY KEY,
              username VARCHAR NOT NULL,
              email VARCHAR NOT NULL,
              password VARCHAR NOT NULL,
              bio VARCHAR,
              image VARCHAR NULL,
              CONSTRAINT unique_username UNIQUE (username),
              CONSTRAINT unique_email UNIQUE (email)
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS article (
              id INTEGER PRIMARY KEY,
              slug VARCHAR NOT NULL,
              title VARCHAR NOT NULL,
              description VARCHAR NOT NULL,
              body VARCHAR NOT NULL,
              created_at TIMESTAMP NOT NULL,
              updated_at TIMESTAMP NOT NULL,
              author INTEGER NOT NULL REFERENCES user ON DELETE RESTRICT ON UPDATE RESTRICT,
              CONSTRAINT unique_slug UNIQUE (slug)
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS tag (
              id INTEGER PRIMARY KEY,
              name VARCHAR NOT NULL,
              CONSTRAINT unique_tag_name UNIQUE (name)
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS article_tag (
              tagid INTEGER NOT NULL REFERENCES tag ON DELETE RESTRICT ON UPDATE RESTRICT,
              articleid INTEGER NOT NULL REFERENCES article ON DELETE RESTRICT ON UPDATE RESTRICT,
              CONSTRAINT articles_with_tag UNIQUE (tagid, articleid),
              CONSTRAINT tags_of_article UNIQUE (articleid, tagid)
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS comment (
              id INTEGER PRIMARY KEY,
              created_at TIMESTAMP NOT NULL,
              updated_at TIMESTAMP NOT NULL,
              body VARCHAR NOT NULL,
              article INTEGER NOT NULL REFERENCES article ON DELETE RESTRICT ON UPDATE RESTRICT,
              author INTEGER NOT NULL REFERENCES user ON DELETE RESTRICT ON UPDATE RESTRICT
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS favorite (
              userid INTEGER NOT NULL REFERENCES user ON DELETE RESTRICT ON UPDATE RESTRICT,
              articleid INTEGER NOT NULL REFERENCES article ON DELETE RESTRICT ON UPDATE RESTRICT,
              CONSTRAINT unique_favorite UNIQUE (articleid, userid)
            )
          |]
        , [sql|
            CREATE TABLE IF NOT EXISTS follow (
              follower INTEGER NOT NULL REFERENCES user ON DELETE RESTRICT ON UPDATE RESTRICT,
              followee INTEGER NOT NULL REFERENCES user ON DELETE RESTRICT ON UPDATE RESTRICT,
              CONSTRAINT unique_follow UNIQUE (follower, followee)
            )
          |]
        ]

  mapM_ (execute_ conn) ddls
