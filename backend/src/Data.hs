{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs             #-}
module Data where

import           Control.Monad.Trans
import           Data.UUID

newtype UserId = UserId UUID deriving Show
newtype UserName = UserName String deriving Show
newtype About = About String deriving Show

data User = User {
    userId   :: UserId,
    userName :: UserName,
    about    :: About
  } deriving Show


data DbOperations = InsertUser User deriving Show

class Monad m => MonadDb m where
   runQuery :: DbOperations -> m ()

   default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => DbOperations -> m ()
   runQuery q = lift $ runQuery q
