{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..), AppT ) where

import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           Data.Aeson.Types
import           Data.Maybe                 (listToMaybe, maybe)
import           Data.Text                  (Text)
import           Data.UUID
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant

type AppT a = AppM Handler a
newtype AppM m a = AppM {
  runAppM :: LoggingT (ReaderT Connection m) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Connection, MonadDb)

data RegisterBody = RegisterBody {
  bodyUserName :: Text,
  bodyPassword :: Text,
  bodyAbout    :: Text
} deriving (Show, Generic)

data ApiUser = ApiUser {
  apiUserId    :: Text,
  apiUserName  :: Text,
  apiUserabout :: Text
} deriving (Show, Generic)

instance ToJSON ApiUser
instance FromJSON RegisterBody where
  parseJSON = withObject "Person" $ \v -> RegisterBody <$> v.: "username" <*> v.: "password" <*> v.: "about"

type APIEndpoints =
  "users" :> ReqBody '[JSON] RegisterBody :> Post '[JSON] ApiUser

routes :: (MonadLogger m, MonadDb m, MonadError ServantErr m) => ServerT APIEndpoints m
routes = registerUser

registerUser :: (MonadLogger m, MonadDb m, MonadError ServantErr m) => RegisterBody -> m ApiUser
registerUser body = do
  logInfoN "POST /users"
  mu <- listToMaybe <$> (runQuery . QueryByName . UserName . bodyUserName $ body)
  maybe undefined (const $ err400 { errBody = "Your request makes no sense to me." }) mu


registerUser' :: MonadDb m => RegisterBody -> m ApiUser
registerUser' body = do
                 insertUser nil
                 userToApi <$> head <$> (runQuery . QueryByName . UserName . bodyUserName $ body)
  where insertUser uuid = runCommand $ InsertUser $ bodyToUser uuid
        bodyToUser uuid = User (UserId uuid) (UserName $ bodyUserName body) (About $ bodyAbout body) (Password $ bodyPassword body)
        userToApi user = ApiUser (toText $ unUserId $ userId user)
                                 (unUserName $ userName user)
                                 (unAbout $ about user)
