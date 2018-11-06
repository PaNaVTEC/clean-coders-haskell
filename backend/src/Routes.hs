{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..)) where

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

newtype AppM a = AppM {
  runAppM :: LoggingT (ReaderT Connection Handler) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Connection, MonadDb, MonadError ServantErr)

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
  maybe undefined throwUserAlreadyExist mu
  where
    throwUserAlreadyExist = (const $ throwError err400 { errBody = "Username already in use." })

registerUser' :: MonadDb m => RegisterBody -> m ApiUser
registerUser' body = do
                 insertUser nil
                 userToApi <$> head <$> (runQuery . QueryByName . UserName . bodyUserName $ body)
  where insertUser uuid = runCommand $ InsertUser $ bodyToUser uuid
        bodyToUser uuid = User (UserId uuid) (UserName $ bodyUserName body) (About $ bodyAbout body) (Password $ bodyPassword body)
        userToApi user = ApiUser (toText $ unUserId $ userId user)
                                 (unUserName $ userName user)
                                 (unAbout $ about user)
