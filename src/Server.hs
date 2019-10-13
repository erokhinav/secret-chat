{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Server
    ( run
    ) where


import Data.Function
import Data.IORef
import Data.Map.Strict (Map)
import Network.Simple.TCP
import Network.Socket
import System.IO
import Text.Parsec
import qualified Control.Exception as E
import qualified Data.Map.Strict as Map

import Types
import Logging


data ServerState = ServerState
        { users :: Map UserName UserDescription
        }


type ServerStateRef = IORef ServerState


data ClientQuery = ClientQueryUserList


makeInitServerState :: IO ServerStateRef
makeInitServerState = newIORef $ ServerState $ Map.empty

-- | Run server on specified port
--
run :: PortNumber -> IO ()
run port = do
    state <- makeInitServerState
    run' port state


run' :: PortNumber -> ServerStateRef -> IO ()
run' port serverState = serve HostAny (show port) $ \(clientSock, clientAddr) -> do
    info $ "Connection from " ++ show clientAddr
    handle <- socketToHandle clientSock ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    startClient handle serverState


startClient :: Handle -> ServerStateRef -> IO ()
startClient handle stateRef =
    myRecv >>= parseClientLogin >>= \case
        Nothing -> info "User disconnected"
        Just serverUser@UserDescription{..} -> do
            atomicModifyIORef stateRef (\s@ServerState{..} ->
                if userName `Map.member` users then
                    (s, False)
                else
                    ( s { users = Map.insert userName serverUser users }
                    , True)
                )
                >>= \case
                    False ->
                        mySend $ "HASCHAT ERROR User '" ++ userName ++ "' already registered"
                    True -> do
                        info $ "User '" ++ userName ++ "' connected"
                        mySend "HASCHAT OK"
                        communicateWithClient userName `E.finally` removeUser userName
  where
    myRecv = do
        hIsEOF handle >>= \case
            True  -> return ""
            False -> hGetLine handle
    mySend = hPutStrLn handle
    removeUser userName = do
        info $ "Remove user from list: '" ++ userName ++ "'"
        modifyIORef stateRef $ \s@ServerState{..} ->
            s { users = Map.delete userName users }
    communicateWithClient userName = fix $ \loop ->
        myRecv >>= parseClientQuery >>= \case
            Nothing -> info $ "Client '" ++ userName ++ "' disconnected"
            Just (ClientQueryUserList) -> do
                readIORef stateRef >>= \ServerState{..} -> mySend $ "HASCHAT " ++ show (Map.elems users)
                loop


parseClientLogin :: String -> IO (Maybe UserDescription)
parseClientLogin "" = return Nothing
parseClientLogin message =
    case parse parseClientLogin' "response" message of
        Left err -> fail (show err)
        Right res -> return (Just res)
  where
    parseClientLogin' = do
        string "HASCHAT"
        space
        userName <- many1 alphaNum
        space
        hostName <- manyTill anyChar space
        port <- read <$> many1 digit
        space
        (_pk::String) <- manyTill alphaNum eof
        return $ UserDescription userName hostName port


parseClientQuery :: String -> IO (Maybe ClientQuery)
parseClientQuery "" = return Nothing
parseClientQuery message =
    case parse parseClientQuery' "response" message of
        Left err -> fail (show err)
        Right res -> return (Just res)
  where
    parseClientQuery' = do
        string "HASCHAT"
        space
        string "USERS"
        return ClientQueryUserList


