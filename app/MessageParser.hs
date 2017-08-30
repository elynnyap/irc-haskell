{- Constants and methods for parsing each IRC message -}

module MessageParser(
  Message(..),
  maxMsgSize,
  msgDelimiter,
  getMsg,
  joinMsg
) where

import Data.List.Split

-- A message is either complete (marked by delimiter) or incomplete.
data Message = 
    Complete String | 
    Incomplete String
    deriving (Eq, Show)

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
