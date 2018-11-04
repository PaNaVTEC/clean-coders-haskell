{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
module Data where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text
import           Data.UUID
import           Database.PostgreSQL.Simple

newtype UserId = UserId UUID deriving Show
newtype UserName = UserName Text deriving Show
newtype About = About Text deriving Show

data User = User {
    userId   :: UserId,
    userName :: UserName,
    about    :: About
  } deriving Show


data DbOperations = InsertUser User | QueryByName UserName deriving Show

class Monad m => MonadDb m where
   runQuery :: DbOperations -> m ()

   default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => DbOperations -> m ()
   runQuery q = lift $ runQuery q

instance MonadDb m => MonadDb (ExceptT a m)
instance MonadDb m => MonadDb (LoggingT m)

instance MonadIO m => MonadDb (ReaderT Connection m) where
  runQuery :: DbOperations -> ReaderT Connection m ()
  runQuery q = do
    conn <- ask
    liftIO $ execute_ conn (toSql q)
    return ()
    where
      toSql :: DbOperations -> Query
      toSql (QueryByName name) = undefined
      toSql (InsertUser user)  = undefined
