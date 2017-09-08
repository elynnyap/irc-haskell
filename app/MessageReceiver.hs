{-Methods for receiving messages according to the IRC protocol.-}

module MessageReceiver(
  getFullMsgs
) where

import Network.Socket
import MessageParser

-- Receives full messages.
getFullMsgs :: Socket -> IO [Message]
getFullMsgs sock = parseMsgs <$> input
  where input = listenForMsgs sock "" 

-- Receives data until the last char is a CRLF 
listenForMsgs :: Socket -> String -> IO String 
listenForMsgs sock buffer 
  | lastTwoChars == msgDelimiter = return buffer
  | otherwise = do
      str <- recv sock maxMsgSize
      listenForMsgs sock (buffer ++ str)
  where lastTwoChars = drop (length buffer - 2) buffer
