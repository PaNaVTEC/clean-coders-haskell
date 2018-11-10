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
import           Database.PostgreSQL.Simple
import           IdGenerator
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Routes
import           Servant                              (Application, Handler,
                                                       Proxy (..), ServantErr,
                                                       hoistServer, serve)

app :: (MonadLogger m, MonadDb User m, MonadDb Post m, MonadError ServantErr m, MonadIdGenerator m) => (forall a. m a -> Handler a) -> Application
app nt = logStdoutDev $ serve proxy $ hoistServer proxy nt routes
  where proxy = Proxy :: Proxy APIEndpoints

startApp :: IO ()
startApp = do
  conn <- prodConn
  run 4321 $ app (ntAppT conn)
  where
    ntAppT :: Connection -> AppM a -> Handler a
    ntAppT conn appM = runReaderT (runStderrLoggingT (runAppM appM)) conn

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "cleancoders_haskell"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
