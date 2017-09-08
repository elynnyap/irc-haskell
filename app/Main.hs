module Main where

import Network.Socket
import Control.Concurrent
import ReplyCodes
import MessageReceiver
import Server
import IRCData
import ReplyGenerator
import Network.HostName
import Data.Maybe
import Control.Applicative
import Options

data MainOptions = MainOptions { optPort :: Int, optPasswrd :: String }

instance Options MainOptions where
    defineOptions = pure MainOptions 
        <*> defineOption optionType_int (\o -> o { optionShortFlags = ['p'] }) 
        <*> defineOption optionType_string (\o -> o { optionShortFlags = ['o'] }) 

main :: IO ()
main = runCommand $ \opts args -> do
    sock <- socket AF_INET Stream defaultProtocol -- create socket
    setSocketOption sock ReuseAddr 1 -- make socket immediately reusable
    bind sock (SockAddrInet (toEnum $ optPort opts) iNADDR_ANY) -- listen on TCP port 4242
    listen sock 2 -- set a max of 2 queued connections
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock -- accept a connection and handle it
    forkIO (runConn conn) -- spawn new thread for each connection
    mainLoop sock       -- repeat

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sockAddr) = do
    (clientHostname, _) <- getNameInfo [] True False sockAddr
    let clientHost = fromMaybe (show sockAddr) clientHostname 
    registerUser sock clientHost 
    close sock

registerUser :: Socket -> String -> IO ()
registerUser sock clientHost = do
    userDetails <- getUserDetails sock Nothing Nothing
    case userDetails >>= createUser of
        Nothing -> do
            send sock "error, nickname already in use\r\n"
            registerUser sock clientHost
        Just u -> do 
            hostname <- getHostName
            send sock $ getRPL_WELCOMEReply hostname clientHost u 
            return ()
