{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Maybe                 (listToMaybe)
import           Database.PostgreSQL.Simple (Connection, execute, query)
import           Models

class Monad m => MonadDbWrite a m where
  runCommand :: a -> m ()

  default runCommand :: (MonadDbWrite a m', MonadTrans t, t m' a ~ m a) => a -> m ()
  runCommand a = lift $ runCommand a

instance MonadDbWrite a m => MonadDbWrite a (ExceptT a m)
instance MonadDbWrite a m => MonadDbWrite a (LoggingT m)

class Monad m => MonadDbRead a b m where
  queryMany :: b -> m [a]

  queryOne :: b -> m (Maybe a)
  queryOne b = listToMaybe <$> queryMany b

  default queryMany :: (MonadDbRead a b m', MonadTrans t, t m' a ~ m a) => b -> m [a]
  queryMany = lift . queryMany

instance MonadDbRead a b m => MonadDbRead a b (ExceptT a m)
instance MonadDbRead a b m => MonadDbRead a b (LoggingT m)

data UserDbQueries =
  UserByName UserName
  | UserById UserId

instance MonadIO m => MonadDbRead User UserDbQueries (ReaderT Connection m) where
  queryMany :: UserDbQueries -> ReaderT Connection m [User]
  queryMany q = do
    conn <- ask
    liftIO $ toSql conn q
    where
      toSql :: Connection -> UserDbQueries -> IO [User]
      toSql conn (UserByName (UserName n)) = query conn "SELECT * FROM users WHERE userName = ?" [n]
      toSql conn (UserById (UserId n)) = query conn "SELECT * FROM users WHERE userId = ?" [n]

data UserDbWrites = InsertUser User
instance MonadIO m => MonadDbWrite UserDbWrites (ReaderT Connection m) where
  runCommand :: UserDbWrites -> ReaderT Connection m ()
  runCommand (InsertUser user) = do
    conn <- ask
    _ <- liftIO $ execute conn "INSERT INTO users VALUES (?, ?, ?, ?)" (userId user, userName user, about user, password user)
    return ()

data PostDbQueries = PostsByUserId UserId
instance MonadIO m => MonadDbRead Post PostDbQueries (ReaderT Connection m) where
  queryMany :: PostDbQueries -> ReaderT Connection m [Post]
  queryMany q = do
    conn <- ask
    liftIO $ toSql conn q
    where
      toSql :: Connection -> PostDbQueries -> IO [Post]
      toSql conn (PostsByUserId (UserId n)) = query conn "SELECT * FROM posts WHERE userId = ?" [n]

data PostDbWrites = InsertPost
instance MonadIO m => MonadDbWrite PostDbWrites (ReaderT Connection m) where
  runCommand :: PostDbWrites -> ReaderT Connection m ()
  runCommand = undefined

type MonadDb a b c m = (MonadDbRead a b m, MonadDbWrite c m)

type UserMonadDb m = MonadDb User UserDbQueries UserDbWrites m
type UserMonadDbRead m = MonadDbRead User UserDbQueries m
type UserMonadDbWrite m = MonadDbWrite UserDbWrites m

type PostMonadDb m = MonadDb Post PostDbQueries PostDbWrites m
type PostMonadDbRead m = MonadDbRead Post PostDbQueries m
type PostMonadDbWrite m = MonadDbWrite PostDbWrites m
