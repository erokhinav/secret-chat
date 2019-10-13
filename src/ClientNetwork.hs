{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ClientNetwork
    ( run
    , NetworkAPI(..)
    ) where


import Control.Concurrent
import Control.Exception (bracket, catch, finally, SomeException)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Either
import Data.Function
import Data.IORef
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time.Clock.POSIX
import Network.Simple.TCP
import Network.Socket (PortNumber)
import System.IO
import Text.InterpolatedString.Perl6 (qc)
import Text.Parsec as P
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import ChanWithHistory
import ClientState
import Consts
import Crypto
import Logging
import Types
import Utils


-- | API to network
--
data NetworkAPI = NetworkAPI
        { getCurrentOnlineUsers :: IO [UserDescription]
        , sendMessageToRoom     :: RoomName -> ChatMessage -> IO ()
        , sendInviteToUser      :: RoomName -> UserName -> IO ()
        , createNewRoom         :: RoomName -> IO () -- TODO возможно, перенести в clientState-функции
        , getRoomMessages       :: RoomName -> IO (Maybe (History, IO ChatMessage))
        }


type ServerHandle = Handle
type ClientHandle = Handle


type ClientHandles = Map UserName ClientHandle


type ClientHandlesRef = IORef ClientHandles

type RoomUpdator = RoomName -> ClientRoom -> IO ()


modifyRef :: IORef a -> (a -> a) -> IO ()
modifyRef ref f = atomicModifyIORef' ref (\x -> (f x, ()))


data InviteGenPhase = CollectingPublicKeysPhase | GeneratingKeysPhase | ReadyPhase deriving Show


data OtherClientMessage = OtherClientMessage RoomName ChatMessage
                        | OtherClientInvite RoomName InviteGenPhase [UserName] [UserName] [PublicNumber]
                        deriving Show


-- | Run client network (incoming connections listener) on specified port
--
run :: ClientStateRef -> RoomUpdator -> UserName -> HostName -> PortNumber -> HostName -> PortNumber -> (NetworkAPI -> IO ()) -> IO ()
run clientStateRef roomUpdator myUserName myHostName myPortNumber serverHost serverPort action = do
    serverHandle <- connectToServer myUserName myHostName myPortNumber serverHost serverPort
    clientHandlesRef <- newIORef Map.empty
    bracket
        (forkIO $ runListener clientHandlesRef roomUpdator myUserName myPortNumber clientStateRef serverHandle)
        (killThread)
        (\_ -> action $ NetworkAPI
                { getCurrentOnlineUsers = getCurrentOnlineUsers'                                             serverHandle
                , sendMessageToRoom     = sendMessageToRoom'     clientHandlesRef roomUpdator clientStateRef serverHandle
                , sendInviteToUser      = sendInviteToUser'      clientHandlesRef roomUpdator clientStateRef serverHandle
                , createNewRoom         = createNewRoom'                                      clientStateRef
                , getRoomMessages       = getRoomMessages'                                    clientStateRef
                })


runListener :: ClientHandlesRef -> RoomUpdator -> UserName -> PortNumber -> ClientStateRef -> ServerHandle -> IO ()
runListener clientHandlesRef roomUpdator myUserName port clientStateRef serverHandle =
    serve HostAny (show port) $ \(clientSock, clientAddr) -> do
        info [qc|Connection from {clientAddr}|]
        handle <- sockToHandle clientSock -- TODO here we need to ask user name!
        otherUserName <- hGetLine handle
        info [qc|Connection from {clientAddr} : {otherUserName}|]
        modifyRef clientHandlesRef $ Map.insert otherUserName handle
        (processOtherClient clientHandlesRef roomUpdator myUserName handle clientStateRef serverHandle)
            `finally` (do
                debug [qc|Removing user {otherUserName} from handles...|]
                modifyRef clientHandlesRef $ Map.delete otherUserName
                )


getCurrentOnlineUsers' :: ServerHandle -> IO [UserDescription]
getCurrentOnlineUsers' serverHandle = do
    debug "Ask server for online users"
    hPutStrLn serverHandle "HASCHAT USERS"
    response <- hGetLine serverHandle
    let prefix = take 8 response
        users = read $ drop 8 response
    when (prefix /= "HASCHAT ") $ fail "Bad response from server"
    debug $ "Online users: " ++ show users
    return users


createNewRoom' :: ClientStateRef -> RoomName -> IO ()
createNewRoom' stateRef roomName = do
    debug $ "Create new room '" ++ roomName ++ "'"
    modifyMVar stateRef (\cs@ClientState{..} ->
        if roomName `Map.member` clientRooms then
            return (cs, Just $ "Room '" ++ roomName ++ "' aleardy created") -- TODO just throw error
        else do
            room <- initRoomState clientName
            return (cs { clientRooms = Map.insert roomName room clientRooms }, Nothing)
        )
        >>= \case
            Nothing -> return ()
            Just err -> fail err


getRoomMessages' :: ClientStateRef -> RoomName -> IO (Maybe (History, IO ChatMessage))
getRoomMessages' stateRef roomName = readMVar stateRef >>= \ClientState{..} ->
    (getHistoryAndFetcher . clientRoomMessages)
    `traverse`
    (Map.lookup roomName clientRooms)


sendInviteToUser' :: ClientHandlesRef -> RoomUpdator -> ClientStateRef -> ServerHandle -> RoomName -> UserName -> IO ()
sendInviteToUser' clientHandlesRef roomUpdator stateRef serverHandle roomName invitedUserName = do
    debug $ "Invite user '" ++ invitedUserName ++ "' to '" ++ roomName ++ "'"
    when (roomName /= defaultRoomName) $
        readMVar stateRef >>= \ClientState{..} -> do
            case Map.lookup roomName clientRooms of
                Nothing -> warn $ "No room '" ++ roomName ++ "' for user '" ++ clientName ++ "'"
                Just (clientRoom@ClientRoom{..}) ->
                    if Set.member invitedUserName invitedUsers then
                        warn $ "User '" ++ invitedUserName ++ "' already invited"
                    else do
                        startInviteProcess clientHandlesRef roomUpdator stateRef serverHandle roomName invitedUserName clientRoom


sendMessageToRoom' :: ClientHandlesRef -> RoomUpdator -> ClientStateRef -> ServerHandle -> RoomName -> ChatMessage -> IO ()
sendMessageToRoom' clientHandlesRef roomUpdator stateRef serverHandle roomName chmsg@ChatMessage{..} = readMVar stateRef >>= \ClientState{..} -> do
    debug [qc|Post to room '{roomName}' message '{message}'|]
    case Map.lookup roomName clientRooms of
        Nothing -> warn [qc|No room {roomName}|]
        Just (ClientRoom{..}) -> do
            if (roomName /= defaultRoomName) && (isLeft dhSharedKey) && (Set.size invitedUsers > 1) then
                warn [qc|Crypto key for room {roomName} is not generated|]
            else do
                push clientRoomMessages chmsg
                usersToSent <-
                        if roomName == defaultRoomName then
                            (Set.fromList . map userName) <$> getCurrentOnlineUsers' serverHandle
                        else
                            return invitedUsers
                let encodedMsg = BSU.fromString message
                encryptedMsg' <-
                    if (roomName == defaultRoomName) || (Set.size invitedUsers == 1) then
                        return (Right encodedMsg)
                    else
                        case Crypto.encrypt ((\(Right sk) -> sk) dhSharedKey) encodedMsg of
                            Left err -> return $ Left [qc|Crypto error: {err}|]
                            Right encryptedMessage ->
                                return (Right encryptedMessage)
                case encryptedMsg' of
                    Left err -> warn err
                    Right encryptedMsg -> do
                        let userName' = userName user
                            time' = show $ ((round $ utcTimeToPOSIXSeconds time) :: Int)
                        sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle usersToSent
                            [qc|HASCHAT MESSAGE {roomName} {userName'} {time'} {Base64.encode encryptedMsg}|]


sendMessageToRoomInternal :: ClientHandlesRef -> RoomUpdator -> ClientStateRef -> ServerHandle -> Set UserName -> ByteString -> IO ()
sendMessageToRoomInternal clientHandlesRef roomUpdator clientStateRef serverHandle usersToSent message = do
    debug [qc|Internal: post message '{message}' to users: {Set.toList usersToSent}|]
    readMVar clientStateRef >>= \ClientState{..} -> do
        readIORef clientHandlesRef >>= \clientHandles -> do
            forM_ (clientName `Set.delete` usersToSent) $ \userName -> do
                (maybe (connectToOtherClientByName clientName userName) (return . Just) (Map.lookup userName clientHandles))
                    >>= \case
                        Nothing -> warn [qc|Can't connect to '{userName}', skip sending messages|]
                        Just handle -> do
                            debug [qc|Internal: send '{message}'|]
                            (do BSC8.hPutStrLn handle message
                                debug [qc|Internal: message '{message}' sended|]
                                )
                                `catch`
                                (\(e::SomeException) -> debug [qc|Can't send message to '{userName}': {e}|])
  where
    connectToOtherClientByName myUserName userName' = do
        onlineUsers <- getCurrentOnlineUsers' serverHandle
        case filter ((userName' ==) . userName) onlineUsers of
            (ud:_) -> do
                debug [qc|Try connect to {userName'}|]
                handle <- connectToOtherClient myUserName ud
                modifyRef clientHandlesRef $ Map.insert userName' handle
                return $ Just handle
            [] -> do
                debug [qc|No online user {userName'}|]
                return Nothing
    connectToOtherClient myUserName UserDescription{..} = do
        mhndl <- newEmptyMVar
        forkIO $ connect userHost (show userPort) $ \(otherClientSock, otherClientAddr) -> do
            info [qc|Connected to other client on {otherClientAddr}|]
            hndl <- sockToHandle otherClientSock
            hPutStrLn hndl myUserName
            putMVar mhndl hndl
            (processOtherClient clientHandlesRef roomUpdator myUserName hndl clientStateRef serverHandle)
                `finally` (do
                    debug [qc|Removing user {userName} from handles (2)...|]
                    modifyRef clientHandlesRef $ Map.delete userName
                    )
        takeMVar mhndl


connectToServer :: UserName -> HostName -> PortNumber -> HostName -> PortNumber -> IO ServerHandle
connectToServer myUserName myHostName myPortNumber serverHost serverPort = do
    lock <- newEmptyMVar
    forkIO $
        connect serverHost (show serverPort) $ \(serverSock, serverAddr) -> do
            info $ "Connected to server on " ++ show serverAddr
            handle <- sockToHandle serverSock
            hPutStrLn handle $ unwords ["HASCHAT", myUserName, myHostName, show myPortNumber, "pk"]
            hGetLine handle >>= \case
                "HASCHAT OK" -> do
                    putMVar lock (Right handle)
                    waitForever -- prevent socket close
                err -> do
                    putMVar lock (Left err)
    takeMVar lock >>= either fail return


processOtherClient :: ClientHandlesRef -> RoomUpdator -> UserName -> Handle -> ClientStateRef -> ServerHandle -> IO ()
processOtherClient clientHandlesRef roomUpdator myUserName handle stateRef serverHandle = fix $ \loop ->
    recvQuery >>= parseOtherClientQuery >>= \case
        Nothing -> return ()
        Just (OtherClientInvite  roomName phase reInvitedUsers processedUsers keys)
            -> processInvite roomName phase reInvitedUsers processedUsers keys >> loop
        Just (OtherClientMessage roomName message)
            -> processMessage roomName message >> loop
  where
    recvQuery =
        hIsEOF handle >>= \case
            True -> do
                debug [qc|User disconnected|]
                return ""
            False -> do
                line <- hGetLine handle
                debug [qc|INPUT: {line}|]
                return line
    processMessage roomName msg = do
        debug [qc|Processing message: {msg}|]
        withMVar stateRef $ \ClientState{..} -> do
            case Map.lookup roomName clientRooms of
                Nothing -> warn [qc|Message to unknown room|]
                Just (ClientRoom{..}) -> do
                    case dhSharedKey of
                        Left _ ->
                            if roomName /= defaultRoomName then
                                warn [qc|Room cryptokeys still not synchronized|]
                            else
                                push clientRoomMessages (msg { message = BSU.toString $ Base64.decodeLenient $ BSC8.pack $ message msg })
                        Right dhSharedKey' ->
                            case Crypto.decrypt dhSharedKey' (Base64.decodeLenient (BSC8.pack $ message msg)) of
                                Left err -> do
                                    warn [qc|Can't decode message: {err}|]
                                    push clientRoomMessages (msg { message = [qc|Can't decode message: {err}|]})
                                Right bsmsg -> do
                                    let decrmsg = BSU.toString bsmsg
                                    debug [qc|Decoded message: '{decrmsg}'|]
                                    push clientRoomMessages (msg { message = decrmsg })
    processInvite roomName phase reInvitedUsers processedUsers keys = do
        debug [qc|Inviting: processInvite {roomName} {phase} {reInvitedUsers} {processedUsers} {keys}|]
        clientRoom <-
            modifyMVar stateRef $ \cs@ClientState{..} ->
                case Map.lookup roomName clientRooms of
                    Just cr -> return (cs, cr)
                    Nothing -> do
                        newRoom <- initRoomState myUserName
                        return ( cs { clientRooms = Map.insert roomName newRoom clientRooms }
                               , newRoom)
        let privKey = dhPrivNumber clientRoom
            nextKeys = map (flip calculateNextPublicKey privKey) keys ++ [calculateFirstPublicKey privKey]
            myKey = head nextKeys
        -- debug [qc|Inviting: My private key: {privKey}|]
        case phase of
            CollectingPublicKeysPhase -> do
                if null reInvitedUsers then do
                    -- Первый цикл закончился, дальше цикл догенерации ключей;
                    -- каждая следующая нода формирует свой ключ
                    debug [qc|Inviting: Public transferring complete. Starting GeneratingKeysPhase|]
                    -- debug [qc|Inviting: My shared key: {myKey} (first!)|]
                    setRoomSharedKey (\_ -> Left $ makeSharedKey myKey)
                    sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle (Set.singleton (head processedUsers))
                        [qc|HASCHAT INVITE {roomName::String} 1 {[myUserName]} {tail processedUsers} {showKeys $ tail nextKeys}|]
                else do
                    -- Цикл обмена открытыми ключами продолжается
                    debug [qc|Inviting: Continue public key transferring|]
                    sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle (Set.singleton (head reInvitedUsers))
                        [qc|HASCHAT INVITE {roomName} 0 {tail reInvitedUsers} {processedUsers ++ [myUserName]} {showKeys nextKeys}|]
                    -- Таймер можно не устанавливать, всё равно придёт сообщение READY
            GeneratingKeysPhase -> do
                -- debug [qc|Inviting: My shared key generated: {myKey}|]
                debug [qc|Inviting: My shared key generated!|]
                if null processedUsers then do
                    -- Цикл генерации ключей закончился; разослать всем READY
                    debug [qc|Inviting: Generating keys cycle finished. Starting READY phase|]
                    let newUsersInRoom = myUserName:reInvitedUsers
                    setNewUsersInRoom newUsersInRoom
                    setRoomSharedKey (\_ -> Right $ makeSharedKey myKey)
                    updateRoom
                    sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle (Set.fromList reInvitedUsers)
                        [qc|HASCHAT INVITE {roomName} 2 {newUsersInRoom} [] []|]
                else do
                    -- Цикл генерации продолжается
                    debug [qc|Inviting: Continue key generating|]
                    setRoomSharedKey (\_ -> Left $ makeSharedKey myKey)
                    sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle (Set.singleton (head processedUsers))
                        [qc|HASCHAT INVITE {roomName} 1 {myUserName:reInvitedUsers} {tail processedUsers} {showKeys $ tail nextKeys}|]
            ReadyPhase -> do
                setNewUsersInRoom reInvitedUsers
                setRoomSharedKey (\case { Left key -> Right key; k -> k })
                updateRoom
                debug [qc|READY TO USE NEW KEY!|]
                {- -- Debug output:
                withMVar stateRef $ \ClientState{..} -> do
                    let (Just (Right cr)) = sharedNumber <$> Map.lookup roomName clientRooms
                    debug [qc|READY TO USE NEW KEY {cr}|]
                    let msg = "Check this message!"
                    debug [qc|Init message {msg}|]
                    let encryptedMsg = Crypto.encrypt cr msg
                        decryptedMsg = Crypto.decrypt cr =<< encryptedMsg
                    debug [qc|Encrypted message {encryptedMsg}|]
                    debug [qc|Decrypted message {decryptedMsg}|]
                    -}
                return ()
      where
        showKeys :: [PublicNumber] -> [Integer]
        showKeys = map (\(PublicNumber pn) -> pn)
        setNewUsersInRoom newUsersInRoom' = do
            let newUsersInRoom = Set.fromList newUsersInRoom'
            modifyMVar_ stateRef $ \cs@ClientState{..} ->
                case Map.lookup roomName clientRooms of -- TODO alterF
                    Nothing -> do
                        -- user has invited to new room
                        debug [qc|Inviting: user have invited to new room {roomName}|]
                        newRoom <- initRoomState' newUsersInRoom []
                        return $ cs { clientRooms = Map.insert roomName newRoom clientRooms }
                    Just clientRoom -> do
                        -- update users in room
                        debug [qc|Inviting: update users ({newUsersInRoom}) have invited to new room {roomName}|]
                        let clientRoom' = clientRoom { invitedUsers = newUsersInRoom }
                        return $ cs { clientRooms = Map.insert roomName clientRoom' clientRooms }
        setRoomSharedKey f =
            modifyMVar_ stateRef $ \cs@ClientState{..} ->
                return $ cs { clientRooms = Map.adjust (\cr@ClientRoom{..} -> cr { dhSharedKey = f dhSharedKey } ) roomName clientRooms }
        updateRoom =
            withMVar stateRef $ \ClientState{..} ->
                maybe (return ())
                      (roomUpdator roomName)
                      (Map.lookup roomName clientRooms)


readList' :: (Read a) => P.Parsec String u [a]
readList' = do
    list <- char '[' *> manyTill anyChar (char ']')
    return (read ("[" ++ list ++ "]"))


parseOtherClientQuery :: String -> IO (Maybe OtherClientMessage)
parseOtherClientQuery "" = return Nothing
parseOtherClientQuery rawMsg =
    case parse parseOtherClientQuery' "response" rawMsg of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            fail (show err)
        Right res -> return (Just res)
  where
    parseOtherClientQuery' = do
        string "HASCHAT"
        space
        res <- parseOtherClientInvite' <|> parseOtherClientMessage'
        eof
        return res
    parseOtherClientInvite' = do
        string "INVITE"
        space
        roomName <- many1 alphaNum
        space
        phase' <- many1 digit
        space
        nextReInvitedUserNames <- readList'
        space
        waitingForKeyUserNames <- readList'
        space
        keys <- readList'
        let phase = case (read phase')::Int of
                      0 -> CollectingPublicKeysPhase
                      1 -> GeneratingKeysPhase
                      _ -> ReadyPhase
        return $ OtherClientInvite roomName phase nextReInvitedUserNames waitingForKeyUserNames (map PublicNumber keys)
    parseOtherClientMessage' = do
        string "MESSAGE"
        space
        roomName <- many1 alphaNum
        space
        userName <- many1 alphaNum
        space
        (time::Integer) <- read <$> many1 digit
        space
        message <- manyTill anyChar eof
        --
        let utcTime = posixSecondsToUTCTime (fromIntegral time)
        let user = UserDescription userName "?" 5000
        let msg = ChatMessage user utcTime message
        --
        return $ OtherClientMessage roomName msg


-- * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- * INVITING
-- * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


startInviteProcess :: ClientHandlesRef -> RoomUpdator -> ClientStateRef -> ServerHandle -> RoomName -> UserName -> ClientRoom -> IO ()
startInviteProcess clientHandlesRef roomUpdator stateRef serverHandle roomName invitedUserName clientRoom = do
    -- Я инициатор.
    debug [qc|Inviting: Start inviting to room '{roomName}'|]
    -- Пользователи комнаты (включая самого себя)
    myUserName <- clientName <$> readMVar stateRef
    let reInvitedUsers = invitedUserName : ((Set.toList (myUserName `Set.delete` (invitedUsers clientRoom))))
    debug [qc|Inviting: re-invited users {reInvitedUsers} / {invitedUsers clientRoom}|]
    -- 1. Creating first client private key: `g^a (mod p)`
    -- debug [qc|Inviting: My private key: {privNumber clientRoom}|]
    let (PublicNumber ga) = calculateFirstPublicKey (dhPrivNumber clientRoom)
    -- 2. Послать сообщение первому приглашённому;
    sendMessageToRoomInternal clientHandlesRef roomUpdator stateRef serverHandle (Set.singleton (head reInvitedUsers))
        [qc|HASCHAT INVITE {roomName} 0 {tail reInvitedUsers} {[myUserName]} {[ga]}|]
