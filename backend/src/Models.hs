{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models where

import           Data.Text                            (Text)
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple           (Connection)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           IdGenerator                          (UUID)

newtype UserId = UserId { unUserId :: UUID } deriving (Eq, Show, Generic, FromField, ToField)
newtype UserName = UserName { unUserName :: Text } deriving (Eq, Show, Generic, FromField, ToField)
newtype About = About { unAbout :: Text } deriving (Eq, Show, Generic, FromField, ToField)
newtype Password = Password { unPassword :: Text } deriving (Eq, Show, Generic, FromField, ToField)

data User = User {
  userId   :: UserId,
  userName :: UserName,
  about    :: About,
  password :: Password
} deriving (Eq, Show, Generic)
instance FromRow User

instance ToRow User where
  toRow user = [
     toField . userId $ user,
     toField . userName $ user,
     toField . about $ user]

newtype PostId = PostId { unPostId :: UUID } deriving (Eq, Show, Generic, FromField, ToField)

data Post = Post {
  postId     :: PostId,
  postUserId :: UserId,
  postText   :: Text,
  postDate   :: UTCTime
} deriving (Eq, Show, Generic)
instance FromRow Post where

data AppInput = AppInput { badWords :: [Text] }
data ReadOnlyState = ReadOnlyState { connection :: Connection, appInput :: AppInput }
