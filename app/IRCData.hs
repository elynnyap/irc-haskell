{-Methods and datatypes for all data that must be stored by the IRC server.-}

module IRCData (
  Nickname,
  Username,
  Fullname,
  Channel,
  User(..),
  Session(..),
  Directory(..),
  newDirectory,
  addUser,
  removeUser,
  nickIsTaken
) where

import Network.Socket
import Data.List (delete)
import Data.HashMap.Strict
import Data.Hashable
import Data.IORef

type Nickname = String
type Username = String
type Fullname = String
type Channel = String

data User = User {
    nickname :: Nickname,
    username :: Username,
    fullname :: Fullname,
    channels :: [Channel]
} deriving (Show)

instance Eq User where
    user1 == user2 = nickname user1 == nickname user2

instance Ord User where
    user1 `compare` user2 = nickname user1 `compare` nickname user2

instance Hashable User where
    hashWithSalt n user = hashWithSalt n (nickname user)

data Session = Session {
    user :: User,
    clientHostname :: String,
    serverHostname :: String, 
    sock :: Socket,
    directory :: IORef Directory
}

newtype Directory = Directory { users :: HashMap Nickname User }

newDirectory :: Directory
newDirectory = Directory empty

addUser :: User -> Directory -> Directory
addUser user Directory{users=users} = Directory users'  
    where users' = insert (nickname user) user users 

removeUser :: User -> Directory -> Directory
removeUser user Directory{users=users} = Directory users' 
    where users' = Data.HashMap.Strict.delete (nickname user) users

nickIsTaken :: Nickname -> Directory -> Bool
nickIsTaken nick dir = member nick (users dir)
