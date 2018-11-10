{-# LANGUAGE FlexibleContexts #-}
module PostsService where

import           Data
import           Data.Maybe (listToMaybe, maybe)

data GetWallError = UserIdDoesNotExist

getPostsByUserId :: (MonadDb Post m, MonadDb User m) => UserId -> m (Either GetWallError [Post])
getPostsByUserId _userId = do
  mu <- listToMaybe <$> runQuery (QueryById _userId)
  maybe
    (return $ Left UserIdDoesNotExist)
    (fmap Right . getPosts)
    (userId <$> mu)

getPosts :: MonadDb Post m => UserId -> m [Post]
getPosts _userId = runQuery $ GetPostsByUserId _userId
