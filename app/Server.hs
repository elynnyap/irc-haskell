{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  createUser
)
where

import Control.Applicative ((<|>))
import Data.HashSet
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing, fromJust)
import IRCData
import MessageParser
import MessageReceiver
import Network.Socket

createUser :: Socket -> Maybe Nickname -> Maybe Fullname -> Maybe Username -> IORef (HashSet String) -> IO User
createUser sock nick fullname username reg_nicks = 
    if isNothing nick || isNothing fullname || isNothing username then do
        msgs <- getFullMsgs sock 
        (nick', fullname', username') <- processUserReg msgs
        let n'  = nick' <|> nick
            fn' = fullname' <|> fullname
            u'  = username' <|> username
        createUser sock n' fn' u' reg_nicks
    else return $ User (fromJust nick) (fromJust fullname) (fromJust username) []

processUserReg :: [Message] -> IO (Maybe Nickname, Maybe Fullname, Maybe Username)
processUserReg = Data.List.foldl' f' (Nothing, Nothing, Nothing) 
    where cat = category msg
          f' (n, fn, u) msg   
              | cat == "NICK" = (Just $ head (params msg), fn, u)
              | cat == "USER" = (n, Just $ params msg !! 3, Just $ head $ params msg)
              | otherwise = (n, fn, u)
