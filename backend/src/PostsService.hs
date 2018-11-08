module PostsService where

import           Data

getPostsByUserId :: UserId -> m (Either String [Post])
getPostsByUserId _userId = undefined
