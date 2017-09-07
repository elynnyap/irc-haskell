{-Module for generating replies sent by IRC server to client(s)-}
module ReplyGenerator (
    getRPL_WELCOMEReply
) where

import IRCData
import ReplyCodes
import MessageParser

getRPL_WELCOMEReply :: String -> String -> User -> String
getRPL_WELCOMEReply hostname clienthost User{nickname=n} =
    hostname ++ " " ++ rpl_welcome ++ " " ++ n ++ 
    " :Welcome to the Internet Rely Network " ++ n ++ "!" ++ n 
    ++ "@" ++ clienthost  ++ msgDelimiter

