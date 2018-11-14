{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Stubs where
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Writer
import           Data
import           Data.List                 (filter, intercalate)
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (UTCTime (..))
import           IdGenerator
import           Models
import           MonadTime                 (MonadTime, currentUTCTime)
import           Servant                   (Handler, ServantErr)
import           Text.Printf

data GlobalState = GlobalState {
  uuidSeed :: Integer,
  users    :: [User],
  posts    :: [Post],
  time     :: UTCTime
}

newtype TestM a = TestM {
    runTestM :: WriterT [String] (StateT GlobalState Handler) a
} deriving (
  Functor,
  Applicative,
  Monad,
  MonadIO,
  MonadState GlobalState,
  MonadError ServantErr)

instance MonadDbRead User UserDbQueries TestM where
  queryMany (UserByName name)  = gets $ filter ((== name) . userName) . users
  queryMany (UserById _userId) = gets $ filter ((== _userId) . userId) . users

instance MonadDbWrite UserDbWrites TestM where
  runCommand (InsertUser user) = modify $ \s -> s { users = users s ++ [user]}

instance MonadDbRead Post PostDbQueries TestM where
  queryMany (PostsByUserId _userId) = gets $ filter ((== _userId) . postUserId) . posts
  queryMany (PostById _postId) = gets $ filter ((== _postId) . postId) . posts

instance MonadDbWrite PostDbWrites TestM where
  runCommand (InsertPost _post) = modify $ \s -> s { posts = posts s ++ [_post]}

instance MonadIdGenerator TestM where
  generateUUID = do
    modify $ \s -> s { uuidSeed = uuidSeed s + 1 }
    gets $ \s -> fromMaybe nilUUID (uuidFromInt . uuidSeed $ s)

uuidFromInt :: Integer -> Maybe UUID
uuidFromInt = fromString . hyphenise . left32Pad
  where
    hyphenise :: String -> String
    hyphenise x = intercalate "-" (splitPlaces [8, 4, 4, 4, 12] x)

    left32Pad :: Integer -> String
    left32Pad = printf "%032d"

splitPlaces :: [Int] -> String -> [String]
splitPlaces = go []
  where
    go :: [String] -> [Int] -> String -> [String]
    go acc _ ""          = acc
    go acc [] _          = acc
    go acc (p : ps) _str = go (acc ++ [take p _str]) ps (drop p _str)

instance MonadLogger TestM where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]

instance MonadTime TestM where
  currentUTCTime = gets time
