{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Lib ( startApp , app ) where

import           Control.Monad.Logger
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Routes
import           Servant

app :: (MonadLogger m) => (forall a. m a -> Handler a) -> Application
app nt = logStdoutDev $ serve proxy $ hoistServer proxy nt routes
  where proxy = (Proxy :: Proxy APIEndpoints)

startApp :: IO ()
startApp = do
  run 4321 $ app ntAppT
  where
    ntAppT :: AppT a -> Handler a
    ntAppT appT = runStderrLoggingT . runAppM $ appT
