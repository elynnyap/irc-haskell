{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  getUserDetails
)
where

import Network.Socket
import MessageReceiver
import MessageParser
import IRCData
import Data.Maybe (isNothing)

-- Parses messages for user's details. Returns Nothing if message
-- format does not match specs.
getUserDetails :: Socket -> Maybe Nickname -> Maybe Fullname -> IO (Maybe (Nickname, Fullname))
getUserDetails sock nick fullname = 
    case isNothing nick || isNothing fullname of
        True -> do
            msgs <- getFullMsgs sock 
            let nick = (!! 1) <$> extractParams nickmsg (getMessagePattern "NICK") 
                fullname = (!! 4) <$> extractParams namemsg (getMessagePattern "USER") 
            getUserDetails sock nick fullname
        False -> return $ (,) <$> nick <*> fullname
