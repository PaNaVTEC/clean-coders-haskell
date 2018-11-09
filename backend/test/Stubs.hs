{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stubs where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Writer
import           Data
import           Data.List                 (filter)
import           IdGenerator
import           Servant

newtype TestM a = TestM {
    runTestM :: WriterT [String] (StateT [User] Handler) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState [User], MonadError ServantErr)

instance MonadDb TestM where
  runQuery (QueryByName name) = gets $ filter (\user -> userName user == name)
  runCommand (InsertUser user) = modify (++ [user])

instance MonadIdGenerator TestM where
  generateUUID = return nilUUID

instance MonadLogger TestM where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]
