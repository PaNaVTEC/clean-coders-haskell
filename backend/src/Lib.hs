{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Lib ( startApp , app ) where

import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           IdGenerator
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Routes
import           Servant

app :: (MonadLogger m, MonadDb m, MonadError ServantErr m, MonadIdGenerator m) => (forall a. m a -> Handler a) -> Application
app nt = logStdoutDev $ serve proxy $ hoistServer proxy nt routes
  where proxy = (Proxy :: Proxy APIEndpoints)

startApp :: IO ()
startApp = do
  conn <- prodConn
  run 4321 $ app (ntAppT conn)
  where
    ntAppT :: Connection -> AppM a -> Handler a
    ntAppT conn appM = runReaderT (runStderrLoggingT (runAppM appM)) conn

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "sample"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
