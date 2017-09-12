{-Module for generating replies sent by IRC server to client(s)-}
module ReplyGenerator (
    getRPL_WELCOME,
    getRPL_YOURHOST,
    getRPL_CREATED,
    getRPL_MYINFO,
    getERR_NICKNAMEINUSE,
    getERR_ALREADYREGISTRED,
    getQUITReply
) where

import IRCData
import ReplyCodes
import MessageParser

header :: String -> String
header hostname = ":" ++ hostname ++ " "

versionNum :: String
versionNum = "1.0"

userModes :: String
userModes = "ao"

channelModes :: String
channelModes = "mtov"

getRPL_WELCOME :: String -> String -> User -> String
getRPL_WELCOME hostname clienthost user =
    header hostname ++ rpl_welcome ++ " " ++ n ++ 
    " :Welcome to the Internet Relay Network " ++ n ++ "!" ++ u 
    ++ "@" ++ clienthost ++ msgDelimiter
    where n = nickname user
          u = username user

getRPL_YOURHOST :: String -> String
getRPL_YOURHOST hostname = header hostname ++ rpl_yourhost ++ " Your host is " ++ hostname ++ ", running version " ++ versionNum ++ msgDelimiter

getRPL_CREATED :: String -> String
getRPL_CREATED hostname = header hostname ++ rpl_created ++ " This server was created 09-21-2017" ++ msgDelimiter 

getRPL_MYINFO :: String -> String
getRPL_MYINFO hostname = header hostname ++ rpl_myinfo ++ " " ++ hostname ++ " " ++ versionNum ++ " " ++ userModes ++ " " ++ channelModes ++ msgDelimiter

getERR_NICKNAMEINUSE :: Nickname -> String
getERR_NICKNAMEINUSE nick = nick ++ " :Nickname is already in use" ++ msgDelimiter

getQUITReply :: String -> String -> String
getQUITReply clientHost msg = "Closing Link: " ++ clientHost ++ " (" ++ msg ++ ")" ++ msgDelimiter

getERR_ALREADYREGISTRED :: String
getERR_ALREADYREGISTRED = ":Unauthorized command (already registered)" ++ msgDelimiter
