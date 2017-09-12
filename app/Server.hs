{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  createUser,
  runSession
)
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.HashSet
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing, fromJust)
import IRCData
import MessageParser
import MessageReceiver
import Network.Socket
import ReplyGenerator

createUser :: Socket -> Maybe Nickname -> Maybe Fullname -> Maybe Username -> IORef Directory -> IO User
createUser sock nick fullname username directory = 
    if isNothing nick || isNothing fullname || isNothing username then do
        msgs <- getFullMsgs sock 
        (nick', fullname', username') <- processUserReg msgs directory sock
        let n'  = nick' <|> nick
            fn' = fullname' <|> fullname
            u'  = username' <|> username
        createUser sock n' fn' u' directory
    else do
        let newUser = User (fromJust nick) (fromJust username) (fromJust fullname) []
            f dir = (addUser newUser dir, ())
        atomicModifyIORef' directory f
        return newUser

processUserReg :: [Message] -> IORef Directory -> Socket -> IO (Maybe Nickname, Maybe Fullname, Maybe Username)
processUserReg msgs directory sock = Data.List.foldl' f' (pure (Nothing, Nothing, Nothing)) msgs
    where f' tup msg = do  
            (n, fn, u) <- tup
            case category msg of
                "NICK" -> do
                    let nick = head $ params msg
                    nickExists <- nickIsTaken nick <$> readIORef directory 
                    if nickExists then do
                        send sock $ getERR_NICKNAMEINUSE nick
                        return (n, fn, u)
                    else return (Just nick, fn, u)
                "USER" -> return (n, Just $ params msg !! 3, Just $ head $ params msg)
                _ -> return (n, fn, u)

runSession :: Session -> IO () 

runSession session = do
    msgs <- getFullMsgs $ sock session
    continueSession <- processMsgs session msgs 
    when continueSession $ runSession session

-- Processes a list of messages, returning True if session should continue
processMsgs :: Session -> [Message] -> IO Bool
processMsgs _ [] = return True
processMsgs session (msg:msgs) = case category msg of
    "QUIT" -> do
        processQuit session msg
        return False
    _ -> do
        process session msg
        processMsgs session msgs

processQuit :: Session -> Message -> IO () 
processQuit Session{sock=sock, user=user, clientHostname=clientHostname, directory=directory} msg = do
    let f dir = (removeUser user dir, ())
    atomicModifyIORef' directory f
    send sock $ getQUITReply clientHostname (head $ params msg) 
    close sock
    return ()

process :: Session -> Message -> IO ()
process Session{sock=sock} msg = case category msg of
    "USER" -> do
        send sock getERR_ALREADYREGISTRED
        return ()
    _ -> return ()
