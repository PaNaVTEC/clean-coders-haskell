{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostsService (
  GetWallError(..),
  getPostsByUserId,
  postToTimeline,
  PostToTimelineError(..),
  UserIdDoesNotExist(..)) where

import           Data
import           Data.Text
import           IdGenerator
import           Models
import           MonadTime   (MonadTime, currentUTCTime)

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

data PostToTimelineError = PostToTimelineError UserIdDoesNotExist | MessageNotPosted

postToTimeline ::
  (UserMonadDbRead m, PostMonadDb m, MonadIdGenerator m, MonadTime m) =>
  UserId -> Text -> m (Either PostToTimelineError Post)
postToTimeline _userId _text = do
  (mu :: Maybe User) <- queryOne (UserById _userId)
  maybe
    (return . Left $ PostToTimelineError UserIdDoesNotExist)
    (const $ queryMessage =<< saveMessage)
    mu
  where
    saveMessage = do
      _postId <- PostId <$> generateUUID
      now <- currentUTCTime
      runCommand $ InsertPost (Post _postId _userId _text now)
      return _postId

    queryMessage _postId = maybe
      (Left MessageNotPosted)
      Right
      <$> queryOne (PostById _postId)
