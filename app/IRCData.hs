{-Methods and datatypes for all data that must be stored by the IRC server.-}

module IRCData (
  Nickname,
  Fullname,
  Channel,
  User(..),
  createUser
) where

type Nickname = String
type Fullname = String
type Channel = String

data User = User {
    nickname :: Nickname,
    fullname :: Fullname,
    channels :: [Channel]
} deriving (Eq, Show)

createUser :: (Nickname, Fullname) -> Maybe User
createUser (nick, fullname) = Just $ User nick fullname []
