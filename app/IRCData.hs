{-Methods and datatypes for all data that must be stored by the IRC server.-}

module IRCData (
  Nickname,
  Username,
  Fullname,
  Channel,
  User(..),
) where

type Nickname = String
type Username = String
type Fullname = String
type Channel = String

data User = User {
    nickname :: Nickname,
    username :: Username,
    fullname :: Fullname,
    channels :: [Channel]
} deriving (Eq, Show)
