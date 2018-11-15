{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application (AppM(..), AppInput(..)) where

import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           IdGenerator
import           Models
import           MonadTime
import           Servant                   (Handler, ServantErr)

newtype AppM a = AppM {
  runAppM :: LoggingT (ReaderT ReadOnlyState Handler) a
} deriving (
  Functor,
  Applicative,
  Monad,
  MonadIO,
  MonadLogger,
  MonadReader ReadOnlyState,
  MonadDbRead User UserDbQueries,
  MonadDbWrite UserDbWrites,
  MonadDbRead Post PostDbQueries,
  MonadDbWrite PostDbWrites,
  MonadError ServantErr,
  MonadIdGenerator,
  MonadTime
  )
