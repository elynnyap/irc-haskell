{-Methods for receiving messages according to the IRC protocol.-}

module MessageReceiver(
  getFullMsgs
) where

import Network.Socket
import MessageParser

-- Receives a full message.
getFullMsgs :: Socket -> IO [Message]
getFullMsgs sock = listenForMsgs sock $ Incomplete ""

-- Receives data until the last char is a CRLF 
listenForMsgs :: Socket -> [Message] -> IO [Message]
listenForMsgs sock msgs 
  | isComplete (last msgs) = return msgs
  | otherwise = do
      str <- recv sock maxMsgSize
      let newMsg = 
    
listenForMsgs sock (Complete msg) = return $ Complete msg 
listenForMsgs sock (Incomplete msg) = do
    str <- recv sock maxMsgSize
    let newMsg = getMsg str 
        combinedMsgs = joinMsg (Incomplete msg) (getMsg str)
    listenForMsgs sock combinedMsgs
