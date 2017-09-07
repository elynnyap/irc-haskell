{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  getUserDetails
)
where

import Network.Socket
import MessageReceiver
import MessageParser
import IRCData

-- Parses messages for user's details. Returns Nothing if message
-- format does not match specs.
getUserDetails :: Socket -> IO (Maybe (Nickname, Fullname))
getUserDetails sock = do
    send sock "Enter your user nickname.\n"
    nickmsg <- getFullMsg sock 
    send sock "Enter your fullname.\n"
    namemsg <- getFullMsg sock
    let nick = (!! 1) <$> extractParams nickmsg (getMessagePattern "NICK") 
        fullname = (!! 4) <$> extractParams namemsg (getMessagePattern "USER") 
    return $ (,) <$> nick <*> fullname
