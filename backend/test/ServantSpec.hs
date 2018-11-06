{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module ServantSpec (main, spec) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data
import           Data.Text            (Text)
import           Data.UUID
import           Lib
import           Servant
import           Stubs
import           Test.Hspec
import           Test.Hspec.Wai       hiding (post)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec :: Spec
spec =
    with (anAppWith [anUser nil "used" "" ""]) $ do
    describe "POST users" $ do
     it "fail with 400 if username is in use" $ do
      post
        "/users"
        [json|{username: "used", password: "", about: ""}|]
          `shouldRespondWith`
        "Username already in use." {matchStatus = 400}

post path = request "POST" path headers
  where headers =  [("Content-Type", "application/json")]

anAppWith :: Monad m => [User] -> m Application
anAppWith users = return $ app nt
  where
    nt :: TestM a -> Handler a
    nt appM = evalStateT (fst <$> runWriterT (runTestM appM)) users

anUser :: UUID -> Text -> Text -> Text -> User
anUser _id name _about _password = User (UserId _id) (UserName name) (About _about) (Password _password)
