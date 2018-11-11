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
import           Data.Bifunctor            (first)
import           Data.List                 (filter)
import           IdGenerator
import           Models
import           Servant                   (Handler, ServantErr)

type GlobalState = ([User], [Post])
newtype TestM a = TestM {
    runTestM :: WriterT [String] (StateT GlobalState Handler) a
} deriving (
  Functor,
  Applicative,
  Monad,
  MonadIO,
  MonadState GlobalState,
  MonadError ServantErr)

instance MonadDbRead User UserDbQueries TestM where
  queryMany (UserByName name)  = gets $ filter ((== name) . userName) . fst
  queryMany (UserById _userId) = gets $ filter ((== _userId) . userId) . fst

instance MonadDbWrite UserDbWrites TestM where
  runCommand (InsertUser user) = modify $ first (++ [user])

instance MonadDbRead Post PostDbQueries TestM where
  queryMany (PostsByUserId _userId) = gets $ filter ((== _userId) . postUserId) . snd

instance MonadDbWrite PostDbWrites TestM where
  runCommand _ = undefined

instance MonadIdGenerator TestM where
  generateUUID = return nilUUID

instance MonadLogger TestM where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]
