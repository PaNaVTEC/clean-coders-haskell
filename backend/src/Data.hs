{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text                          (Text, unpack)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

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
   runQuery :: DbOperations -> m [User]

   default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => DbOperations -> m [User]
   runQuery q = lift $ runQuery q

instance MonadDb m => MonadDb (ExceptT a m)
instance MonadDb m => MonadDb (LoggingT m)

data UserDb = UserDb {
    iduser    :: UUID ,
    nameuser  :: Text ,
    aboutuser :: Text
  } deriving (Show, Generic)

instance FromRow UserDb
instance ToRow UserDb where
  toRow user = [
     toField . iduser $ user,
     toField . nameuser $ user,
     toField . aboutuser $ user]

instance MonadIO m => MonadDb (ReaderT Connection m) where
  runQuery :: DbOperations -> ReaderT Connection m [User]
  runQuery q = do
    conn <- ask
    liftIO $ fmap (fmap toUser) $ toSql conn q
    where
      toUser user = User (UserId $ iduser user) (UserName $ nameuser user) (About $ aboutuser user)
      toSql :: Connection -> DbOperations -> IO [UserDb]
      toSql conn (QueryByName (UserName name)) = query conn "SELECT * FROM users WHERE nameuser = ?" name
      toSql conn (InsertUser user)  = undefined

