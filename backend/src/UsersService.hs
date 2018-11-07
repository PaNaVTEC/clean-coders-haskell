{-# LANGUAGE OverloadedStrings #-}

module UsersService where

import           Data
import           Data.Maybe (listToMaybe, maybe)
import           Data.UUID

type RegisterUserRequest = (UserName, Password, About)
data RegisterUserError = UsernameAlreadyInUse | UserNotInserted

registerUser :: MonadDb m => RegisterUserRequest -> m (Either RegisterUserError User)
registerUser body@(_userName, _, _) = do
  mu <- queryUserByName _userName
  maybe (Right <$> registerUser' body) (const . return . Left $ UsernameAlreadyInUse) mu

queryUserByName :: MonadDb m => UserName -> m (Maybe User)
queryUserByName _userName = listToMaybe <$> (runQuery $ QueryByName _userName)

registerUser' :: MonadDb m => RegisterUserRequest -> m User
registerUser' (_userName, _password, _about) = do
  insertUser nil
  mu <- queryUserByName _userName
  return $ maybe (error "User not inserted correctly") id mu
  where
    insertUser :: MonadDb m => UUID -> m ()
    insertUser uuid = runCommand $ InsertUser $ bodyToUser uuid

    bodyToUser :: UUID -> User
    bodyToUser uuid = User (UserId uuid) _userName _about _password
