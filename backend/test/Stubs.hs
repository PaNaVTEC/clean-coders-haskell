{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stubs where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Writer
import           Data
import           Servant

newtype TestM m a = TestM {
    runTestM :: WriterT [String] (StateT [User] m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadState [User])

instance MonadDb (TestM Handler) where
    runQuery (InsertUser user) = modify (++ [user])

instance (Monad m) => MonadLogger (TestM m) where
    monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]
