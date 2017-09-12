{-Module for generating replies sent by IRC server to client(s)-}
module ReplyGenerator (
    getRPL_WELCOME,
    getERR_NICKNAMEINUSE
) where

import IRCData
import ReplyCodes
import MessageParser

getRPL_WELCOME :: String -> String -> User -> String
getRPL_WELCOME hostname clienthost user =
    ":" ++ hostname ++ " " ++ rpl_welcome ++ " " ++ n ++ 
    " :Welcome to the Internet Relay Network " ++ n ++ "!" ++ u 
    ++ "@" ++ clienthost ++ msgDelimiter
    where n = nickname user
          u = username user

getERR_NICKNAMEINUSE :: Nickname -> String
getERR_NICKNAMEINUSE nick = nick ++ " :Nickname is already in use" ++ msgDelimiter
