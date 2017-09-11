{-Methods for IRC server features as indicated by the RFCs 2810-2812.-}

module Server (
  createUser
)
where

import Control.Applicative ((<|>))
import Data.HashSet
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isNothing, fromJust)
import IRCData
import MessageParser
import MessageReceiver
import Network.Socket

createUser :: Socket -> Maybe Nickname -> Maybe Fullname -> Maybe Username -> IORef (HashSet String) -> IO User
createUser sock nick fullname username allNicks = 
    if isNothing nick || isNothing fullname || isNothing username then do
        msgs <- getFullMsgs sock 
        (nick', fullname', username') <- processUserReg msgs allNicks sock
        let n'  = nick' <|> nick
            fn' = fullname' <|> fullname
            u'  = username' <|> username
        createUser sock n' fn' u' allNicks
    else do
        let addNick set = (insert (fromJust nick) set, ())
        atomicModifyIORef' allNicks addNick
        set' <- readIORef allNicks
        print set'
        return $ User (fromJust nick) (fromJust username) (fromJust fullname) []

processUserReg :: [Message] -> IORef (HashSet String) -> Socket -> IO (Maybe Nickname, Maybe Fullname, Maybe Username)
processUserReg msgs nicks sock = Data.List.foldl' f' (pure (Nothing, Nothing, Nothing)) msgs
    where f' tup msg = do  
            (n, fn, u) <- tup
            case category msg of
                "NICK" -> do
                    let nick = head $ params msg
                    nickExists <- member nick <$> readIORef nicks 
                    putStrLn $ "nickExists: " ++ show nickExists
                    if nickExists then do
                        send sock "error, nick already registered"
                        return (n, fn, u)
                    else return (Just nick, fn, u)
                "USER" -> return (n, Just $ params msg !! 3, Just $ head $ params msg)
                _ -> return (n, fn, u)

