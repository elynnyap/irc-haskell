{-Module for generating replies sent by IRC server to client(s)-}
module ReplyGenerator (
    getRPL_WELCOMEReply
) where

import IRCData
import ReplyCodes
import MessageParser

getRPL_WELCOMEReply :: String -> String -> User -> String
getRPL_WELCOMEReply hostname clienthost user =
    ":" ++ hostname ++ " " ++ rpl_welcome ++ " " ++ n ++ 
    " :Welcome to the Internet Relay Network " ++ n ++ "!" ++ u 
    ++ "@" ++ clienthost ++ msgDelimiter
    where n = nickname user
          u = username user

