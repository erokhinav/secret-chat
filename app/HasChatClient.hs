-- | Client CLI
--
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Control.Concurrent
import Control.Monad
import Data.List.Split
import Data.Time.Clock
import System.Environment
import System.Exit
import qualified Data.Map.Strict as Map
import Network.Socket
import Text.InterpolatedString.Perl6 (qc)

import ChatWindow as W
import ClientCLI
import ClientNetwork as Net
import ClientState
import Consts
import Database
import Logging
import Types


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ do
        putStrLn "Run:"
        putStrLn "haschat-client <user-name> <own-address:own-port> <server-address:server-port>"
        putStrLn "Example:"
        putStrLn "haschat-client john 127.0.0.1:5001 192.168.1.1:64242"
        exitFailure
    --
    let (myUserName:myAddress:serverAddress:_) = args
        (myHost, myPort) = parseAddr myAddress
        (serverHost, serverPort) = parseAddr serverAddress
        myUserDesc  = UserDescription myUserName myHost myPort
    --
    withLogging False [qc|haschat-client-user-{myUserName}.log|] $
        withDb [qc|haschat-{myUserName}.db|] $ \conn -> do
            clientState' <- loadState conn myUserName
            clientState <- if defaultRoomName `Map.member` (clientRooms clientState')
                    then return clientState'
                    else do
                        defRoom <- initRoomState myUserName
                        return $ clientState' { clientRooms = Map.insert defaultRoomName defRoom (clientRooms clientState') }
            clientStateRef <- newMVar clientState
            Net.run clientStateRef (saveRoom conn) myUserName myHost myPort serverHost serverPort $ \net -> do
                let chatCommands = ChatCommandsEx (getCurrentOnlineUsers net) (sendInviteToUser net) (runChatWindow' conn net myUserDesc) (saveRoom conn)
                runChat myUserName chatCommands clientStateRef


runChatWindow' :: Connection -> NetworkAPI -> UserDescription -> RoomName -> IO ()
runChatWindow' conn net userDesc myRoomName =
    getRoomMessages net myRoomName >>= \case
        Nothing -> putStrLn [qc|No chat info for room "{myRoomName}"|]
        Just (history, fetchMessage) -> do
            inputChan <- newChan
            outputChan <- newChan
            -- input messages from other client
            void $ forkIO $ forever $ do
                msg <- fetchMessage
                saveMessage conn myRoomName msg
                writeChan inputChan msg
            -- output messages from current user
            void $ forkIO $ forever $ do
                ownMsg <- readChan outputChan
                now <- getCurrentTime
                let msg = ChatMessage userDesc now ownMsg
                sendMessageToRoom net myRoomName msg
            -- run chat window
            W.runChatWindow myRoomName (userName userDesc) history inputChan outputChan


parseAddr :: String -> (HostName, PortNumber)
parseAddr addr = let (host:port:_) = splitOn ":" addr in (host, read port)

