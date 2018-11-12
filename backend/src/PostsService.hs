{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostsService where

import           Data
import           Data.Text
import           IdGenerator
import           Models

data UserIdDoesNotExist = UserIdDoesNotExist
data GetWallError = GetWallError UserIdDoesNotExist

getPostsByUserId ::
  (UserMonadDbRead m, PostMonadDbRead m) =>
  UserId -> m (Either GetWallError [Post])
getPostsByUserId _userId = do
  mu <- queryOne (UserById _userId)
  maybe
    (return . Left $ GetWallError UserIdDoesNotExist)
    (fmap Right . getPosts)
    (userId <$> mu)

getPosts :: PostMonadDbRead m => UserId -> m [Post]
getPosts _userId = queryMany $ PostsByUserId _userId

data PostTotimelineError = PostTotimelineError UserIdDoesNotExist | MessageNotPosted

postToTimeline ::
  (UserMonadDbRead m, PostMonadDb m, MonadIdGenerator m) =>
  UserId -> Text -> m (Either PostTotimelineError Post)
postToTimeline _userId _text = do
  (mu :: Maybe User) <- queryOne (UserById _userId)
  maybe
    (return . Left $ PostTotimelineError UserIdDoesNotExist)
    (const $ queryMessage =<< saveMessage)
    mu
  where
    saveMessage = do
      _postId <- PostId <$> generateUUID
      runCommand $ InsertPost (Post _postId _userId _text _)
      return _postId

    queryMessage _postId = do
      mp <- queryOne (PostById _postId)
      return $ maybe
        (Left $ MessageNotPosted)
        Right
        mp
