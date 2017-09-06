{- Constants and methods for parsing each IRC message -}

module MessageParser(
  Message(..),
  maxMsgSize,
  msgDelimiter,
  getMsg,
  joinMsg,
  getMessagePattern,
  extractParams 
) where

import Data.List.Split
import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import IRCData

-- A message is either complete (marked by delimiter) or incomplete.
data Message = 
    Complete String | 
    Incomplete String
    deriving (Eq, Show)

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

-- Extracts the message from a String
-- A single message is a string of max 512 chars, 
-- including the crlf pair `\r\n`. Longer messages are truncated.
getMsg :: String -> Message
getMsg str
  | length tokens > 1 = Complete $ take msgBodyLength body
  | otherwise         = Incomplete $ take msgBodyLength body
  where tokens = splitOn msgDelimiter str 
        body = head tokens

-- Concatenates two messages if the first is incomplete
joinMsg :: Message -> Message -> Message
joinMsg (Complete x) _ = Complete x -- do nothing if first is already complete
joinMsg (Incomplete x) (Complete y) = Complete $ take msgBodyLength (x ++ y)  
joinMsg (Incomplete x) (Incomplete y) = Incomplete $ take msgBodyLength (x ++ y)

extractNickname :: Message -> Maybe Nickname
extractNickname (Incomplete _) = Nothing
extractNickname (Complete str)
  | cmdIndex == Just 0 && length tokens > 1 = Just $ Nickname $ tokens !! 1
  | otherwise = Nothing
  where cmdIndex = elemIndex "NICK" tokens
        tokens = words str

-- Given a message and its pattern, returns a Just String if the message
-- fulfills the required pattern (i.e. is valid for its type) and a Nothing otherwise
extractParams :: Message -> Maybe MessagePattern -> Maybe [String]
extractParams (Incomplete _) _ = Nothing
extractParams _ Nothing = Nothing 
extractParams (Complete str) (Just (Pattern numParams command delimiter))
  | patternMatches = Just tokens
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
