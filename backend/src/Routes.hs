{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..)) where

import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           Data.Aeson.Types
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           IdGenerator
import           PostsService
import           Servant                    hiding (Post)
import           UsersService               (RegisterUserError (..),
                                             registerUser)

newtype AppM a = AppM {
  runAppM :: LoggingT (ReaderT Connection Handler) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadReader Connection, MonadDb, MonadError ServantErr, MonadIdGenerator)

data RegisterBody = RegisterBody {
  bodyUserName :: Text,
  bodyPassword :: Text,
  bodyAbout    :: Text
} deriving (Show, Generic)

data ApiUser = ApiUser {
  apiUserId    :: UUID,
  apiUserName  :: Text,
  apiUserabout :: Text
} deriving (Show, Generic)

data ApiPost = ApiPost {
  apiPostUserId   :: Text,
  apiPostId       :: Text,
  apiPostText     :: Text,
  apiPostDatetime :: Text
} deriving (Show, Generic)

instance ToJSON ApiPost where
  toJSON post = object [
    "userId"   .= apiPostUserId post,
    "postId"   .= apiPostId post,
    "text"     .= apiPostText post,
    "datetime" .= apiPostDatetime post]

instance ToJSON ApiUser where
  toJSON user = object [
    "id"       .= apiUserId user,
    "username" .= apiUserName user,
    "about"    .= apiUserabout user]

instance FromJSON RegisterBody where
  parseJSON = withObject "Person" $ \v -> RegisterBody <$> v.: "username" <*> v.: "password" <*> v.: "about"

type APIEndpoints =
  "users" :> ReqBody '[JSON] RegisterBody :> PostCreated '[JSON] ApiUser
  :<|> "users" :> Capture "userId" UUID :> "wall" :> Get '[JSON] [ApiPost]

routes :: (MonadLogger m, MonadDb m, MonadIdGenerator m, MonadError ServantErr m) => ServerT APIEndpoints m
routes = registerUserRoute :<|> userWallRoute

userWallRoute :: MonadDb m => UUID -> m [ApiPost]
userWallRoute _userId = do
  ei <- getPostsByUserId (UserId _userId)
  either throwPostsError (return . postToApi) ei
  where
    postToApi :: [Post] -> [ApiPost]
    postToApi _post = undefined
    throwPostsError :: String -> m a
    throwPostsError _a = undefined

registerUserRoute :: (MonadLogger m, MonadDb m, MonadIdGenerator m, MonadError ServantErr m) => RegisterBody -> m ApiUser
registerUserRoute body = do
  ei <- registerUser (registerBody' body)
  either throwRegisterError (return . userToApi) ei
  where
    userToApi :: User -> ApiUser
    userToApi user = ApiUser
      (unUserId $ userId user)
      (unUserName $ userName user)
      (unAbout $ about user)

    registerBody' _body = (UserName $ bodyUserName _body,
                          Password $ bodyPassword _body,
                          About $ bodyAbout _body)

    throwRegisterError UsernameAlreadyInUse = throwError
      err400
      {errBody = "Username already in use."}

    throwRegisterError UserNotInserted = throwError
      err500
      {errBody = "Internal server error."}
