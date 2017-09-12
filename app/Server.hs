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
import Data.Foldable (forM_)

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
    session' <- processMsgs session msgs 
    forM_ session' runSession

-- Processes a list of messages, returning Nothing if session is terminated
processMsgs :: Session -> [Message] -> IO (Maybe Session)
processMsgs session [] = return $ Just session
processMsgs session (msg:msgs) = case category msg of
    "QUIT" -> do
        processQuit session msg
        return Nothing
    _ -> do
        session' <- process session msg
        processMsgs session' msgs

processQuit :: Session -> Message -> IO () 
processQuit Session{sock=sock, user=user, clientHostname=clientHostname, directory=directory} msg = do
    let f dir = (removeUser user dir, ())
    atomicModifyIORef' directory f
    send sock $ getQUITReply clientHostname (head $ params msg) 
    close sock
    return ()

process :: Session -> Message -> IO Session
process session@Session{user=user, clientHostname=clientHostname, serverHostname=serverHostname, sock=sock, directory=directory} msg = case category msg of
    "USER" -> do
        send sock getERR_ALREADYREGISTRED
        return session
    "NICK" -> do
        let newNick = head $ params msg
        nickChanged <- changeNick newNick user directory
        if nickChanged then do
            let user' = user { nickname = newNick }
            return $ session { user = user' }
        else do
            send sock $ getERR_NICKNAMEINUSE newNick
            return session
    _ -> return session
