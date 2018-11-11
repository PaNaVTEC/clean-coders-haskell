{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UsersService where

import           Data
import           Data.Maybe  (fromMaybe, maybe)
import           IdGenerator
import           Models

type RegisterUserRequest = (UserName, Password, About)
data RegisterUserError = UsernameAlreadyInUse | UserNotInserted

registerUser :: (MonadIdGenerator m, UserMonadDb m) => RegisterUserRequest -> m (Either RegisterUserError User)
registerUser body@(_userName, _, _) = do
  mu <- queryUserByName _userName
  maybe (Right <$> registerUser' body) (const . return . Left $ UsernameAlreadyInUse) mu

queryUserByName :: UserMonadDbRead m => UserName -> m (Maybe User)
queryUserByName _userName = queryOne (UserByName _userName)

registerUser' :: (MonadIdGenerator m, UserMonadDb m) => RegisterUserRequest -> m User
registerUser' (_userName, _password, _about) = do
  uuid <- generateUUID
  insertUser uuid
  mu <- queryUserByName _userName
  return $ fromMaybe (error "User not inserted correctly") mu
  where
    insertUser :: UserMonadDb m => UUID -> m ()
    insertUser = runCommand . InsertUser . bodyToUser

    bodyToUser :: UUID -> User
    bodyToUser uuid = User (UserId uuid) _userName _about _password
