{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ServantSpec (main, spec) where

import           Control.Monad.State   (evalStateT)
import           Control.Monad.Writer
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Lazy
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX
import           IdGenerator
import           Lib
import           Models
import           Network.Wai.Test      (SResponse (..))
import           Servant               (Application, Handler)
import           Stubs
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  let state = GlobalState
        0
        [anUser nilUUID "used" "" ""]
        [aPost nilUUID nilUUID "A new post" (posixSecondsToUTCTime 0)]
        (posixSecondsToUTCTime 0)
  in
  with (anAppWith state) $ do
  describe "Register user" $ do

    it "fail with 400 if username is in use" $
      postRegister [json|{username: "used", password: "", about: ""}|]
        `shouldRespondWith`
        "Username already in use." {matchStatus = 400}

    it "returns a new user when the username does not exist" $
       postRegister [json|{username: "aUser", password: "pass", about: "About"}|]
         `shouldRespondWith`
         [json|{id: "00000000-0000-0000-0000-000000000001", username: "aUser", about: "About"}|] {matchStatus = 201}

  describe "User wall" $ do

    it "returns the wall for the specified user" $
      getWallOf "00000000-0000-0000-0000-000000000000"
        `shouldRespondWith`
        [json|[{
             userId: "00000000-0000-0000-0000-000000000000",
             postId: "00000000-0000-0000-0000-000000000000",
             text: "A new post",
             datetime: "1970-01-01T00:00:00Z"
        }]|] {matchStatus = 200}

    it "returns 404 for a bad request" $
      getWallOf "incorrect uuid" `shouldRespondWith` 404

  describe "User timeline" $

    it "posts a new message in the user timeline" $
      postMessage "00000000-0000-0000-0000-000000000000" [json|{text:"Another post"}|]
        `shouldRespondWith`
        [json|{
             userId: "00000000-0000-0000-0000-000000000000",
             postId: "00000000-0000-0000-0000-000000000001",
             text: "Another post",
             datetime: "1970-01-01T00:00:00Z"
        }|] {matchStatus = 201}

postMessage :: String -> ByteString -> WaiSession SResponse
postMessage _userId = request "POST" (BS8.pack $ "/users/" ++ _userId ++ "/timeline") headers
  where headers = [("Content-Type", "application/json")]

getWallOf :: String -> WaiSession SResponse
getWallOf _userId = request "GET" (BS8.pack $ "/users/" ++ _userId ++ "/wall")  [] ""

postRegister :: ByteString -> WaiSession SResponse
postRegister = request "POST" "/users" headers
  where headers = [("Content-Type", "application/json")]

anAppWith :: Monad m => GlobalState -> m Application
anAppWith _users = return $ app nt
  where
    nt :: TestM a -> Handler a
    nt appM = evalStateT (fst <$> runWriterT (runTestM appM)) _users

anUser :: UUID -> Text -> Text -> Text -> User
anUser _id _name _about _password = User
  (UserId _id)
  (UserName _name)
  (About _about)
  (Password _password)

aPost :: UUID -> UUID -> Text -> UTCTime -> Post
aPost _postId _userId = Post (PostId _postId) (UserId _userId)
