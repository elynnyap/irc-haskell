{- Constants and methods for parsing each IRC message -}

module MessageParser(
  Message(..),
  maxMsgSize,
  msgDelimiter,
  parseMsgs 
) where

import Data.List.Split
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import IRCData

data Message = Message {
                 category :: String,
                 params :: [String]
               } deriving Show

-- Defines the pattern for valid messages in the IRC protocol
data MessagePattern = Pattern {
                        numParams :: Int,
                        command :: String,
                        delimiter :: Maybe Char
                       }

getMessagePattern :: String -> Maybe MessagePattern
getMessagePattern "NICK" = Just $ Pattern 1 "NICK" Nothing
getMessagePattern "USER" = Just $ Pattern 4 "USER" $ Just ':'
getMessagePattern "PRIVMSG" = Just $ Pattern 2 "PRIVMSG" $ Just ':'
getMessagePattern "JOIN" = Just $ Pattern 1 "JOIN" Nothing
getMessagePattern "PART" = Just $ Pattern 2 "PART" $ Just ':'
getMessagePattern "QUIT" = Just $ Pattern 1 "QUIT" $ Just ':'
getMessagePattern _ = Nothing

-- Maximum size of each message in bytes
maxMsgSize :: Int
maxMsgSize = 4096

-- Delimiter that indicates the end of a string in a message
msgDelimiter :: String
msgDelimiter = "\r\n"

-- Maximum number of characters in a message body, excluding delimiter
msgBodyLength :: Int
msgBodyLength = 510

-- Scans a string to produce an array of valid messages.
parseMsgs :: String -> [Message]
parseMsgs str = foldr f [] tokens
  where f str msgs = case createMsg str of 
                       Nothing -> msgs
                       Just m -> msgs ++ [m] 
        tokens = map truncate $ splitOn msgDelimiter str
        truncate = take msgBodyLength 

-- Scans a string to produce a Message if it's valid; Nothing otherwise.
createMsg :: String -> Maybe Message
createMsg str 
  | null tokens = Nothing
  | isJust params = Just $ Message cmd (fromJust params)
  | otherwise = Nothing
  where params = extractParams str (getMessagePattern cmd) 
        cmd = head tokens
        tokens = words str

-- Given a string and its message pattern, returns a Just String if the message
-- fulfills the required pattern (i.e. is valid for its type) and a Nothing otherwise
extractParams :: String -> Maybe MessagePattern -> Maybe [String]
extractParams _ Nothing = Nothing 
extractParams str (Just (Pattern numParams command delimiter))
  | patternMatches = Just $ drop 1 tokens
  | otherwise = Nothing
  where patternMatches = isJust tokensResult && numParamsMatches && commandMatches 
        numParamsMatches = numParams == length tokens - 1 
        commandMatches = head tokens == command 
        tokensResult = getTokens str delimiter
        tokens = fromJust tokensResult

-- Returns an array of strings based on the delimiter provided
-- E.g. for the string "USER amy * * :Amy Pond" and delimiter ':',
-- the result is ["User", "amy", "*", "*", "Amy Pond"]
getTokens :: String -> Maybe Char -> Maybe [String]
getTokens input Nothing = Just $ words input
getTokens input (Just sym)  
  | isNothing delimIdx = Nothing
  | otherwise = Just arr
  where delimIdx = elemIndex sym input
        arr = words params ++ [drop 1 body]
        (params, body) = splitAt (fromJust delimIdx) input
