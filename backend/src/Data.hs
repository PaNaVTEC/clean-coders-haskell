{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text                            (Text)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

newtype UserId = UserId UUID deriving (Show, Generic, FromField, ToField)
newtype UserName = UserName Text deriving (Show, Generic, FromField, ToField)
newtype About = About Text deriving (Show, Generic, FromField, ToField)

data User = User {
    userId   :: UserId,
    userName :: UserName,
    about    :: About
  } deriving (Show, Generic)

data DbQueries = QueryByName UserName deriving Show
data DbCommands = InsertUser User deriving Show

class Monad m => MonadDb m where
   runQuery :: DbQueries -> m [User]

   runCommand :: DbCommands -> m ()

   default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => DbQueries -> m [User]
   runQuery q = lift $ runQuery q

   default runCommand :: (MonadDb m', MonadTrans t, t m' ~ m) => DbCommands -> m ()
   runCommand q = lift $ runCommand q

instance MonadDb m => MonadDb (ExceptT a m)
instance MonadDb m => MonadDb (LoggingT m)

instance FromRow User

instance ToRow User where
  toRow user = [
     toField . userId $ user,
     toField . userName $ user,
     toField . about $ user]

instance MonadIO m => MonadDb (ReaderT Connection m) where
  runQuery :: DbQueries -> ReaderT Connection m [User]
  runQuery q = do
    conn <- ask
    liftIO $ toSql conn q
    where
      toSql :: Connection -> DbQueries -> IO [User]
      toSql conn (QueryByName (UserName n)) = query conn "SELECT * FROM users WHERE nameuser = ?" [n]

  runCommand :: DbCommands -> ReaderT Connection m ()
  runCommand (InsertUser user) = do
    conn <- ask
    _ <- liftIO $ execute conn "INSERT INTO users VALUES (?, ?, ?)" (userId user, userName user, about user)
    return ()
