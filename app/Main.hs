module Main where

import Control.Applicative
import Control.Concurrent
import Data.HashSet
import Data.IORef
import Data.Maybe
import IRCData
import MessageReceiver
import Network.HostName
import Network.Socket
import Options
import ReplyCodes
import ReplyGenerator
import Server

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
    directory <- newIORef newDirectory
    mainLoop sock directory

mainLoop :: Socket -> IORef Directory -> IO ()
mainLoop sock directory = do
    conn <- accept sock -- accept a connection and handle it
    forkIO (runConn conn directory) -- spawn new thread for each connection
    mainLoop sock directory      -- repeat

runConn :: (Socket, SockAddr) -> IORef Directory -> IO ()
runConn (sock, sockAddr) directory = do
    (clientHostname, _) <- getNameInfo [] True False sockAddr
    serverHost <- getHostName
    let clientHost = fromMaybe (show sockAddr) clientHostname 
    user <- registerUser sock clientHost serverHost directory
    runSession $ Session user clientHost serverHost sock directory 

registerUser :: Socket -> String -> String -> IORef Directory -> IO User 
registerUser sock clientHost serverHost directory = do
    user <- createUser sock Nothing Nothing Nothing directory
    send sock $ getRPL_WELCOME serverHost clientHost user 
    send sock $ getRPL_YOURHOST serverHost
    send sock $ getRPL_CREATED serverHost
    send sock $ getRPL_MYINFO serverHost
    return user 
