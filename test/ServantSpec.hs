{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module ServantSpec (main, spec) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.ByteString
import           Data.Time.Clock.POSIX
import           Lib
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai        hiding (post)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = with anApp $ do
  describe "POST users" $ do
    it "responds" $ do
      post
        "/users"
        [json|{username: "asdf", password: "asd", about: ""}|]

          `shouldRespondWith`

        200

post path = request "POST" path headers
  where headers =  [("Content-Type", "application/json")]

type LoggingOutput = [String]
newtype TestM m a = TestM {
  runTestM :: WriterT LoggingOutput m a
} deriving (Functor, Applicative, Monad, MonadIO)

anApp :: Monad m => m Application
anApp = return $ app nt
  where
    nt :: TestM Handler a -> Handler a
    nt appM = do
      c <- runWriterT (runTestM appM)
      return $ fst c

instance (Monad m) => MonadLogger (TestM m) where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]
