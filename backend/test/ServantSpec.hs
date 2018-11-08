{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module ServantSpec (main, spec) where

import           Control.Monad.State   (evalStateT)
import           Control.Monad.Writer
import           Data
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Lazy
import           Data.Text             (Text)
import           IdGenerator
import           Lib
import           Network.Wai.Test      (SResponse (..))
import           Servant
import           Stubs
import           Test.Hspec
import           Test.Hspec.Wai        hiding (post)
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec :: Spec
spec =
  with (anAppWith [anUser nilUUID "used" "" ""]) $ do
  describe "Register user" $ do

    it "fail with 400 if username is in use" $ do
      postRegister [json|{username: "used", password: "", about: ""}|]
        `shouldRespondWith`
        "Username already in use." {matchStatus = 400}

    it "returns a new user when the username does not exist" $ do
       postRegister [json|{username: "aUser", password: "pass", about: "About"}|]
         `shouldRespondWith`
         [json|{id: "00000000-0000-0000-0000-000000000000", username: "aUser", about: "About"}|] {matchStatus = 201}

  describe "User wall" $ do
    it "returns the wall for the specified user" $ do
      getWallOf "00000000-0000-0000-0000-000000000001"
        `shouldRespondWith`
        [json|[{
             userId: "00000000-0000-0000-0000-000000000001",
             postId: "00000000-0000-0000-0000-000000000001",
             text: "A new post",
             datetime: "2001-01-01T01:01:01"
        }]|] {matchStatus = 200}

    it "returns 404 for a bad request" $ do
      getWallOf "incorrect uuid" `shouldRespondWith` 404

getWallOf :: [Char] -> WaiSession SResponse
getWallOf _userId = request "GET" (BS8.pack $ "/users/" ++ _userId ++ "/wall")  [] ""

postRegister :: ByteString -> WaiSession SResponse
postRegister = request "POST" "/users" headers
  where headers = [("Content-Type", "application/json")]

anAppWith :: Monad m => [User] -> m Application
anAppWith users = return $ app nt
  where
    nt :: TestM a -> Handler a
    nt appM = evalStateT (fst <$> runWriterT (runTestM appM)) users

anUser :: UUID -> Text -> Text -> Text -> User
anUser _id _name _about _password = User
  (UserId _id)
  (UserName _name)
  (About _about)
  (Password _password)
