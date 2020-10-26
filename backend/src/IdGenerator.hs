{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module IdGenerator (nilUUID, MonadIdGenerator(..), UUID, fromText, fromString) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson.Types                     (FromJSON, ToJSON)
import           Data.Text
import qualified Data.UUID                            as U (UUID, fromString,
                                                            fromText, nil,
                                                            toText)
import           Data.UUID.V4                         (nextRandom)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Servant                              (FromHttpApiData (..))

newtype UUID = UUID {
  unUUID :: Text
} deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)

instance FromHttpApiData UUID where
  parseQueryParam :: Text -> Either Text UUID
  parseQueryParam t = maybe (Left _error) Right (fromText t)
    where _error = "Not valid UUID"

class Monad m => MonadIdGenerator m where
  generateUUID :: m UUID

  default generateUUID :: (MonadIdGenerator m', MonadTrans t, t m' ~ m) => m UUID
  generateUUID = lift generateUUID

instance MonadIO m => MonadIdGenerator (ReaderT a m) where
  generateUUID = liftIO $ toUUID <$> nextRandom

instance MonadIdGenerator IO where
  generateUUID = toUUID <$> nextRandom

instance MonadIdGenerator m => MonadIdGenerator (ExceptT a m)
instance MonadIdGenerator m => MonadIdGenerator (LoggingT m)

toUUID :: U.UUID -> UUID
toUUID = UUID . U.toText

nilUUID :: UUID
nilUUID = toUUID U.nil

fromText :: Text -> Maybe UUID
fromText x = toUUID <$> U.fromText x

fromString :: String -> Maybe UUID
fromString x = toUUID <$> U.fromString x
