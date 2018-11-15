{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Lib ( startApp , app ) where

import           Application
import           Control.Monad.Error.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data
import           Database.PostgreSQL.Simple
import           IdGenerator                          (MonadIdGenerator)
import           Models                               (ReadOnlyState (..))
import           MonadTime                            (MonadTime)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Routes
import           Servant                              (Application, Handler,
                                                       Proxy (..), ServantErr,
                                                       hoistServer, serve)

app :: (MonadLogger m,
        UserMonadDb m,
        PostMonadDb m,
        MonadError ServantErr m,
        MonadIdGenerator m,
        MonadTime m,
        MonadReader ReadOnlyState m) => (forall a. AppInput -> m a -> Handler a) -> AppInput -> Application
app nt _appInput = logStdoutDev $ serve proxy $ hoistServer proxy (nt _appInput) routes
  where proxy = Proxy :: Proxy APIEndpoints

startApp :: IO ()
startApp = do
  conn <- prodConn
  run 4321 $ app (ntAppT conn) _
  where
    ntAppT :: Connection -> AppInput -> AppM a -> Handler a
    ntAppT conn _appInput appM = runReaderT (runStderrLoggingT (runAppM appM)) (ReadOnlyState conn _appInput)

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "cleancoders_haskell"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
