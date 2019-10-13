{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
module Tests.NetSpec where


import Control.Concurrent
import Control.Monad
import Data.Function
import Data.Time.Clock
import Network.Socket
import System.FilePath.Posix
import System.IO.Temp
import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Test.Hspec

import ClientNetwork as Net
import ClientState
import Consts
import Logging
import Server
import Types


spec :: Spec
spec = describe "Network" $ do
    it "clients must exchange messages 1" $ example $
        runTestNetwork
            [ runClient "userA" $ \net user ->
                mkMessage user "Тестовое сообщение" >>= sendMessageToRoom net defaultRoomName
            , runClient "userB" $ \net _user ->
                waitForMessage net "Тестовое сообщение"
            ]
    it "clients must exchange messages 2" $ example $
        runTestNetwork
            [ runClient "userA" $ \net user -> do
                mkMessage user "Hi!" >>= sendMessageToRoom net defaultRoomName
                waitForMessage net "How are you?"
            , runClient "userB" $ \net user -> do
                waitForMessage net "Hi!"
                mkMessage user "How are you?" >>= sendMessageToRoom net defaultRoomName
            ]
    it "can invite other and communicate" $ example $ do
        let testRoomName = "myNewTestRoom"
        userBready <- newEmptyMVar
        runTestNetwork
            [ runClient "userA" $ \net user -> do
                void $ takeMVar userBready
                createNewRoom net testRoomName
                sendInviteToUser net testRoomName "userB"
                threadDelay 1000000
                mkMessage user "Hi!" >>= sendMessageToRoom net testRoomName
            , runClient "userB" $ \net _user -> do
                putMVar userBready ()
                waitForMessage' net testRoomName "Hi!"
            ]
    it "can invite other and communicate with UTF8" $ example $ do
        let testRoomName = "myNewTestRoom"
        userBready <- newEmptyMVar
        runTestNetwork
            [ runClient "userA" $ \net user -> do
                void $ takeMVar userBready
                createNewRoom net testRoomName
                sendInviteToUser net testRoomName "userB"
                threadDelay 1000000
                mkMessage user "Привет!" >>= sendMessageToRoom net testRoomName
                waitForMessage' net testRoomName "Как дела?"
                mkMessage user "Норм!" >>= sendMessageToRoom net testRoomName
            , runClient "userB" $ \net user -> do
                putMVar userBready ()
                waitForMessage' net testRoomName "Привет!"
                mkMessage user "Как дела?" >>= sendMessageToRoom net testRoomName
                waitForMessage' net testRoomName "Норм!"
            ]
    it "can invite some users other and communicate with UTF8" $ example $ do
        let numberOfUsers = 10
        let testRoomName = "myNewTestRoom"
        usersReady <- mapM (const newEmptyMVar) [1..numberOfUsers]
        runTestNetwork $
            [ runClient "userA" $ \net user -> do
                mapM_ takeMVar usersReady
                createNewRoom net testRoomName
                forM_ [1..numberOfUsers] $ \i -> do
                    sendInviteToUser net testRoomName ("user" ++ show i)
                    threadDelay 100000
                mkMessage user "Привет всем!" >>= sendMessageToRoom net testRoomName
                (flip fix) (map (\i -> "Норм " ++ show i) [1..numberOfUsers]) $ \loop restMsgs -> do
                    unless (null restMsgs) $ do
                        msg <- waitForMessage'' net testRoomName (`elem` restMsgs)
                        loop (msg `L.delete` restMsgs)
            ] ++
            ((flip map [1..numberOfUsers]) $ \userNum ->
                runClient ("user" ++ show userNum) $ \net user -> do
                    putMVar (usersReady !! (userNum - 1)) ()
                    waitForMessage' net testRoomName "Привет всем!"
                    mkMessage user ("Норм " ++ show userNum) >>= sendMessageToRoom net testRoomName
                    threadDelay 1000000
                    )


-- * ---


serverPort :: PortNumber
serverPort = 60000


clientFirstPort :: PortNumber
clientFirstPort = 60001


runTestNetwork :: [PortNumber -> IO ()] -> IO ()
runTestNetwork clients =
    withSystemTempDirectory "haschat-log" $ \tmpdir -> do
        withLogging False (tmpdir </> "test.log") $ do
            -- Start server
            serverLock <- newEmptyMVar
            serverThread <- forkIO $ do
                putMVar serverLock ()
                Server.run serverPort
            void $ takeMVar serverLock -- waiting for server thread start
            threadDelay 500000 -- waiting for server start
            clientLocks <- forM (zip clients [clientFirstPort..]) $ \(client, clientPort) -> do
                clientLock <- newEmptyMVar
                void $ forkIO $ do
                    client clientPort
                    putMVar clientLock ()
                return clientLock
            -- Waiting for clients to finish
            mapM_ takeMVar clientLocks
            killThread serverThread


runClient :: UserName -> (NetworkAPI -> UserDescription -> IO ()) -> PortNumber -> IO ()
runClient myUserName action myPort = do
    rooms <- initRoomState myUserName >>= return . Map.singleton defaultRoomName
    clientStateRef <- newMVar $ ClientState rooms myUserName
    Net.run clientStateRef (\_ _ -> return ()) myUserName "localhost" myPort "localhost" serverPort $ \net ->
        action net (UserDescription myUserName "localhost" myPort)


waitForMessage :: NetworkAPI -> String -> IO ()
waitForMessage net expectedMsg = waitForMessage' net defaultRoomName expectedMsg


waitForMessage' :: NetworkAPI -> RoomName -> String -> IO ()
waitForMessage' net roomName expectedMsg = void $ waitForMessage'' net roomName (==expectedMsg)


waitForMessage'' :: NetworkAPI -> RoomName -> (String -> Bool) -> IO String
waitForMessage'' net roomName expectedMsgPred =
    fix $ \loop ->
        getRoomMessages net roomName >>= \case
            Nothing -> do
                threadDelay 100000
                loop
            Just (hist, fetch) ->
                case L.find (expectedMsgPred . message) hist of
                    Just msg -> return (message msg)
                    Nothing ->
                        fix $ \loop' -> fetch >>= \ChatMessage{..} ->
                            if expectedMsgPred message then
                                return message
                            else
                               loop'


mkMessage :: UserDescription -> String -> IO ChatMessage
mkMessage user msg = do
    now <- getCurrentTime
    return $ ChatMessage user now msg

