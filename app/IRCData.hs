{-Methods and datatypes for all data that must be stored by the IRC server.-}

module IRCData (
  Nickname(..),
  Fullname(..),
  createUser
) where

newtype Nickname = Nickname String deriving (Eq, Show)
newtype Fullname = Fullname String deriving (Eq, Show)
newtype Channel = Channel String deriving (Eq, Show)

data User = User {
    nickname :: Nickname,
    fullname :: Fullname,
    channels :: [Channel]
} deriving (Eq, Show)

createUser :: (Nickname, Fullname) -> Maybe User
createUser (nick, fullname) = Just $ User nick fullname []
