module PostsService where

import           Data
import           Data.Maybe (listToMaybe, maybe)

getPostsByUserId :: MonadDb m => UserId -> m (Either String [Post])
getPostsByUserId _userId = do
  mu <- listToMaybe <$> runQuery (QueryById _userId)
  maybe
    (return $ Left "The userId does not exist")
    (fmap Right . getPosts)
    (userId <$> mu)

getPosts :: MonadDb m => UserId -> m [Post]
getPosts _userId = undefined -- runQuery $ GetPostsByUserId _userId
