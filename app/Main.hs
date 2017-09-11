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
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    allNicks <- newIORef (fromList [] :: HashSet String)
    conn <- accept sock -- accept a connection and handle it
    forkIO (runConn conn allNicks) -- spawn new thread for each connection
    mainLoop sock       -- repeat

runConn :: (Socket, SockAddr) -> IORef (HashSet String) -> IO ()
runConn (sock, sockAddr) allNicks = do
    (clientHostname, _) <- getNameInfo [] True False sockAddr
    let clientHost = fromMaybe (show sockAddr) clientHostname 
    registerUser sock clientHost allNicks
    runConn (sock, sockAddr) allNicks

registerUser :: Socket -> String -> IORef (HashSet String) -> IO ()
registerUser sock clientHost allNicks = do
    user <- createUser sock Nothing Nothing Nothing allNicks
    hostname <- getHostName
    send sock $ getRPL_WELCOMEReply hostname clientHost user 
    return ()
