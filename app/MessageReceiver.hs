{-Methods for receiving messages according to the IRC protocol.-}

module MessageReceiver(
  getFullMsg
) where

import Network.Socket
import MessageParser

-- Receives a full message.
getFullMsg :: Socket -> IO Message
getFullMsg sock = listenForMsg sock $ Incomplete ""

-- Receives data until a full message is received.
listenForMsg :: Socket -> Message -> IO Message
listenForMsg sock (Complete msg) = return $ Complete msg 
listenForMsg sock (Incomplete msg) = do
    str <- recv sock maxMsgSize
    let newMsg = getMsg str 
        combinedMsgs = joinMsg (Incomplete msg) (getMsg str)
    listenForMsg sock combinedMsgs
