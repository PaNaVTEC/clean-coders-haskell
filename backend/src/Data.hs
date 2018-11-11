{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text                            (Text)
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           IdGenerator

newtype UserId = UserId { unUserId :: UUID } deriving (Eq, Show, Generic, FromField, ToField)
newtype UserName = UserName { unUserName :: Text } deriving (Eq, Show, Generic, FromField, ToField)
newtype About = About { unAbout :: Text } deriving (Eq, Show, Generic, FromField, ToField)
newtype Password = Password { unPassword :: Text } deriving (Eq, Show, Generic, FromField, ToField)

data User = User {
  userId   :: UserId,
  userName :: UserName,
  about    :: About,
  password :: Password
} deriving (Eq, Show, Generic)
instance FromRow User

instance ToRow User where
  toRow user = [
     toField . userId $ user,
     toField . userName $ user,
     toField . about $ user]

newtype PostId = PostId { unPostId :: UUID } deriving (Eq, Show, Generic, FromField, ToField)

data Post = Post {
  postId     :: PostId,
  postUserId :: UserId,
  postText   :: Text,
  postDate   :: UTCTime
} deriving (Eq, Show, Generic)
instance FromRow Post where

data DbQueries =
  QueryByName UserName
  | QueryById UserId
  | GetPostsByUserId UserId deriving Show

class Monad m => MonadDb a (m :: * -> *) where
   runQuery :: DbQueries -> m [a]

   insert :: a -> m ()

   default runQuery :: (MonadDb a m', MonadTrans t, t m' a ~ m a) => DbQueries -> m [a]
   runQuery q = lift $ runQuery q

   default insert :: (MonadDb a m', MonadTrans t, t m' a ~ m a) => a -> m ()
   insert = lift . insert

instance MonadDb a m => MonadDb a (ExceptT a m)
instance MonadDb a m => MonadDb a (LoggingT m)

instance MonadIO m => MonadDb User (ReaderT Connection m) where
  runQuery :: DbQueries -> ReaderT Connection m [User]
  runQuery q = do
    conn <- ask
    liftIO $ toSql conn q
    where
      toSql :: Connection -> DbQueries -> IO [User]
      toSql conn (QueryByName (UserName n)) = query conn "SELECT * FROM users WHERE userName = ?" [n]
      toSql conn (QueryById (UserId n)) = query conn "SELECT * FROM users WHERE userId = ?" [n]

  insert :: User -> ReaderT Connection m ()
  insert user = do
    conn <- ask
    _ <- liftIO $ execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (userId user, userName user, about user, password user)
    return ()

instance MonadIO m => MonadDb Post (ReaderT Connection m) where
  runQuery :: DbQueries -> ReaderT Connection m [Post]
  runQuery q = do
    conn <- ask
    liftIO $ toSql conn q
    where
      toSql :: Connection -> DbQueries -> IO [Post]
      toSql conn (GetPostsByUserId (UserId n)) = query conn "SELECT * FROM posts WHERE userId = ?" [n]

  insert :: Post -> ReaderT Connection m ()
  insert = undefined
