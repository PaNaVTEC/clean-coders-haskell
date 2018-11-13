{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module MonadTime where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Time            (UTCTime, getCurrentTime)

class Monad m => MonadTime m where
  currentUTCTime :: m UTCTime

  default currentUTCTime :: (MonadTime m', MonadTrans t, t m' ~ m) => m UTCTime
  currentUTCTime = lift currentUTCTime

instance MonadTime m => MonadTime (ExceptT a m)
instance MonadTime m => MonadTime (LoggingT m)

instance MonadIO m => MonadTime (ReaderT a m) where
  currentUTCTime = liftIO getCurrentTime

instance MonadTime IO where
  currentUTCTime = getCurrentTime
