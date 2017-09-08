{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  getUserDetails
)
where

import Network.Socket
import MessageReceiver
import MessageParser
import IRCData
import Data.Maybe (isNothing, fromJust)

-- Parses messages for user's details. Returns Nothing if message
-- format does not match specs.
getUserDetails :: Socket -> Maybe Nickname -> Maybe Fullname -> IO (Nickname, Fullname)
getUserDetails sock nick fullname = 
    if isNothing nick || isNothing fullname then do
        msgs <- getFullMsgs sock 
        let f' msg (n, f)   
              | isNothing n && category msg == "NICK" = (Just $ head (params msg), f)
              | isNothing f && category msg == "USER" = (n, Just $ params msg !! 3)
              | otherwise = (n, f)
            f = foldr f' (nick, fullname) 
            (nick', fullname') = f msgs
        getUserDetails sock nick' fullname'
    else return (fromJust nick, fromJust fullname)
