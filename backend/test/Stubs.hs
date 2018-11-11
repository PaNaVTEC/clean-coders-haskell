{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Stubs where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Writer
import           Data
import           Data.List                 (filter)
import           IdGenerator
import           Servant                   (Handler, ServantErr)

type GlobalState = ([User], [Post])
newtype TestM a = TestM {
    runTestM :: WriterT [String] (StateT GlobalState Handler) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadState GlobalState, MonadError ServantErr)

instance MonadDb User TestM where
  runQuery (QueryByName name) = gets $ filter (\user -> userName user == name) . fst
  runQuery (QueryById _userId) = gets $ filter (\user -> userId user == _userId) . fst
  insert user = modify $ \(users, posts) -> ((++ [user]) users, posts)

instance MonadDb Post TestM where
  runQuery (GetPostsByUserId _userId) = gets $ filter (\post -> postUserId post == _userId) . snd
  insert _ = undefined

instance MonadIdGenerator TestM where
  generateUUID = return nilUUID

instance MonadLogger TestM where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]
