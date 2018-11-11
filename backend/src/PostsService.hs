{-# LANGUAGE FlexibleContexts #-}
module PostsService where

import           Data
import           Data.Maybe (listToMaybe, maybe)

data GetWallError = UserIdDoesNotExist

getPostsByUserId :: (UserMonadDb m, PostMonadDb m) => UserId -> m (Either GetWallError [Post])
getPostsByUserId _userId = do
  mu <- listToMaybe <$> runQuery (UserById _userId)
  maybe
    (return $ Left UserIdDoesNotExist)
    (fmap Right . getPosts)
    (userId <$> mu)

getPosts :: PostMonadDb m => UserId -> m [Post]
getPosts _userId = runQuery $ PostsByUserId _userId
