{-# LANGUAGE FlexibleContexts #-}

module PostsService where

import           Data
import           Models

data GetWallError = UserIdDoesNotExist

getPostsByUserId :: (UserMonadDbRead m, PostMonadDbRead m) => UserId -> m (Either GetWallError [Post])
getPostsByUserId _userId = do
  mu <- queryOne (UserById _userId)
  maybe
    (return $ Left UserIdDoesNotExist)
    (fmap Right . getPosts)
    (userId <$> mu)

getPosts :: PostMonadDbRead m => UserId -> m [Post]
getPosts _userId = queryMany $ PostsByUserId _userId
