{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..), AppT ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           Data.Aeson.Types
import           Data.Text
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant
import           Web.FormUrlEncoded         (FromForm (..), ToForm (..))

type AppT a = AppM Handler a
newtype AppM m a = AppM {
  runAppM :: LoggingT (ReaderT Connection m) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Connection, MonadDb)

data RegisterBody = RegisterBody {
  username :: Text,
  password :: Text,
  about    :: Text
} deriving (Show, Generic)

data ApiUser = ApiUser {
  _id       :: Text,
  _username :: Text,
  _about    :: Text
} deriving (Show, Generic)

instance ToJSON ApiUser
instance FromJSON RegisterBody
instance FromForm RegisterBody
instance ToForm RegisterBody

type APIEndpoints =
  "users" :> ReqBody '[JSON, FormUrlEncoded] RegisterBody :> Post '[JSON] ApiUser

routes :: (MonadLogger m, MonadDb m) => ServerT APIEndpoints m
routes = registerUser

registerUser :: (MonadLogger m, MonadDb m) => RegisterBody -> m ApiUser
registerUser body = do
  runQuery . QueryByName . UserName . username $ body
  logInfoN "POST /users"
  return $ ApiUser "" "" ""
