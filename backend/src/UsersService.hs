{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UsersService where

import           Data
import           Data.Maybe  (fromMaybe, listToMaybe, maybe)
import           IdGenerator

type RegisterUserRequest = (UserName, Password, About)
data RegisterUserError = UsernameAlreadyInUse | UserNotInserted

registerUser :: (MonadIdGenerator m, MonadDb User m) => RegisterUserRequest -> m (Either RegisterUserError User)
registerUser body@(_userName, _, _) = do
  mu <- queryUserByName _userName
  maybe (Right <$> registerUser' body) (const . return . Left $ UsernameAlreadyInUse) mu

queryUserByName :: MonadDb User m => UserName -> m (Maybe User)
queryUserByName _userName = listToMaybe <$> runQuery (QueryByName _userName)

registerUser' :: (MonadIdGenerator m, MonadDb User m) => RegisterUserRequest -> m User
registerUser' (_userName, _password, _about) = do
  uuid <- generateUUID
  insertUser uuid
  mu <- queryUserByName _userName
  return $ fromMaybe (error "User not inserted correctly") mu
  where
    insertUser :: MonadDb User m => UUID -> m ()
    insertUser uuid = runCommand $ InsertUser $ bodyToUser uuid

    bodyToUser :: UUID -> User
    bodyToUser uuid = User (UserId uuid) _userName _about _password
