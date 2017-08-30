module Main where

import Network.Socket
import Control.Concurrent
import ReplyCodes
import MessageParser

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol -- create socket
    setSocketOption sock ReuseAddr 1 -- make socket immediately reusable
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on TCP port 4242
    listen sock 2 -- set a max of 2 queued connections
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock -- accept a connection and handle it
    forkIO (runConn conn) -- spawn new thread for each connection
    mainLoop sock       -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Enter your user nickname.\n"
    username <- getFullMsg sock $ Incomplete "" 
    send sock $ show username
    putStrLn "sent msg"
    close sock

-- Receives data until a full message is received.
getFullMsg :: Socket -> Message -> IO Message
getFullMsg sock (Complete msg) = return $ Complete msg 
getFullMsg sock (Incomplete msg) = do
    str <- recv sock maxMsgSize
    let newMsg = getMsg str 
        combinedMsgs = joinMsg (Incomplete msg) (getMsg str)
    getFullMsg sock combinedMsgs
      
