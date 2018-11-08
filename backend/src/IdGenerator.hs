{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module IdGenerator where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Text
import           Data.UUID                            (nil, toText)
import           Data.UUID.V4                         (nextRandom)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

newtype UUID = UUID { unUUID :: Text } deriving (Eq, Show, FromField, ToField)

class Monad m => MonadIdGenerator m where
  generateUUID :: m UUID

  default generateUUID :: (MonadIdGenerator m', MonadTrans t, t m' ~ m) => m UUID
  generateUUID = lift generateUUID

instance MonadIO m => MonadIdGenerator (ReaderT a m) where
  generateUUID = liftIO $ UUID . toText <$> nextRandom

instance MonadIdGenerator IO where
  generateUUID = UUID . toText <$> nextRandom

instance MonadIdGenerator m => MonadIdGenerator (ExceptT a m)
instance MonadIdGenerator m => MonadIdGenerator (LoggingT m)

nilUUID :: UUID
nilUUID = UUID $ (toText nil)
