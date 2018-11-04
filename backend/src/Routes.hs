{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..), AppT ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson.Types
import           Data.Text
import           GHC.Generics
import           Servant
import           Web.FormUrlEncoded   (FromForm (..), ToForm (..))

type AppT a = AppM Handler a
newtype AppM m a = AppM {
  runAppM :: LoggingT m a
} deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

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

routes :: (MonadLogger m) => ServerT APIEndpoints m
routes = return registerUser

registerUser :: (MonadLogger m) => m ApiUser
registerUser = do
  logInfoN "POST /users"
  return $ ApiUser "" "" ""
