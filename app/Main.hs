module Main where

import Network.Socket
import Control.Concurrent
import ReplyCodes
import MessageReceiver
import Server
import IRCData

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
    registerUser sock
    close sock

registerUser :: Socket -> IO ()
registerUser sock = do
    userDetails <- getUserDetails sock
    let user = userDetails >>= createUser 
    case user of
        Nothing -> do
            send sock "error, nickname already in use"
            registerUser sock
        Just u -> do 
            send sock "user successfully registered." 
            return ()
