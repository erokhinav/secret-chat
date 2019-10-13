{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
module ClientCLI
    ( ChatCommandsEx(..)
    , runChat
    )
    where


import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.State.Strict   (StateT, evalStateT, gets, modify)
import Data.Default                 (def)
import Data.Maybe
import Data.Typeable
import System.Console.StructuredCLI
import qualified Data.Map.Strict as Map
import Text.InterpolatedString.Perl6 (qc)

import ClientState
import Types


data ChatExceptions = ChatExit
        deriving (Show, Typeable)


instance Exception ChatExceptions


data ChatCommandsEx = ChatCommandsEx
        { getCurrentOnlineUsersCLI :: IO [UserDescription]
        , inviteUserCLI            :: RoomName -> UserName -> IO ()
        , runChatWindowCLI         :: RunChatWindow
        , saveRoomCLI              :: RoomName -> ClientRoom -> IO ()
        }


data ClientStateEx = ClientStateEx
        { state        :: ClientStateRef
        , currentRoom  :: Maybe RoomName
        , chatCommands :: ChatCommandsEx
        , chatUserName :: UserName
        }


getx :: (ClientState -> a) -> StateM a
getx f = do
    state'' <- gets state >>= (liftIO . readMVar)
    return (f state'')

modifyx_ :: (ClientState -> IO ClientState) -> StateM ()
modifyx_ f = do
    state' <- gets state
    liftIO $ modifyMVar_ state' f


type StateM = StateT ClientStateEx IO


-- * Root CLI handlers ---------------------------------------------------------


root :: CommandsT StateM ()
root = do
    listRooms
    listUsers
    joinRoom
    createRoom
    quit


listRooms :: CommandsT StateM ()
listRooms = command "rooms" "Show list of rooms" $ do
    rooms <- getx (Map.keys . clientRooms)
    liftIO $ do
        putStrLn "Your rooms:"
        mapM_ putStrLn rooms
    return NoAction


joinRoom :: CommandsT StateM ()
joinRoom = custom "join" "Join room" parseRoom always joinRoomInternal >+ roomCommands
  where
    always = return True
    parseRoom node input = do
        roomsList <- getx (Map.keys . clientRooms)
        parseOneOf roomsList "where to join" node input


joinRoomInternal :: RoomName -> StateT ClientStateEx IO Action
joinRoomInternal roomName = do
    liftIO $ putStrLn $ "Joined into room '" ++ roomName ++ "'..."
    modify (\s -> s { currentRoom = Just roomName })
    return NewLevel


createRoom :: CommandsT StateM ()
createRoom = param "create" "<new room name>" parseRoomName createAndJoinRoom >+ roomCommands
  where
    parseRoomName roomName' = do
        clientRooms' <- getx clientRooms
        liftIO $ do
            if roomName' `Map.member` clientRooms' then do
                liftIO $ putStrLn [qc|Room '{roomName'}' already exists!|]
                return Nothing
            else
                return (Just roomName')
    createAndJoinRoom roomName = do
        chatUserName <- gets chatUserName
        liftIO $ putStrLn [qc|Create room '{roomName}'...|]
        saveRoom <- gets (saveRoomCLI . chatCommands)
        modifyx_ $ \cs@ClientState{..} -> do
            newRoom <- initRoomState chatUserName
            saveRoom roomName newRoom
            return $ cs { clientRooms = Map.insert roomName newRoom clientRooms }
        joinRoomInternal roomName


quit :: CommandsT StateM ()
quit = command "quit" "quit haschat" $ throw ChatExit


-- * Room CLI handlers --------------------------------------------------------


roomCommands :: CommandsT StateM ()
roomCommands = do
    listUsers
    listInvitedUsers
    leaveRoom
    chatInRoom
    inviteUserToRoom
    quit


leaveRoom :: CommandsT StateM ()
leaveRoom = command "leave" "Leave room" $ do
    currentRoomName <- fromJust <$> gets currentRoom
    liftIO $ putStrLn [qc|Leaving room '{currentRoomName}'|]
    modify (\s -> s { currentRoom = Nothing })
    exit


listUsers :: CommandsT StateM ()
listUsers = command "users" "Show list of users" $ do
    getUsers <- gets (getCurrentOnlineUsersCLI . chatCommands)
    liftIO $ do
        putStrLn "Fetching list of current online users..."
        users <- getUsers
        putStrLn "Users:"
        mapM_ (putStrLn . userName) users
    return NoAction


listInvitedUsers :: CommandsT StateM ()
listInvitedUsers = command "invited" "Show list of invited users" $ do
    currentRoomName <- fromJust <$> gets currentRoom
    currentRoom <- getx (fromJust . Map.lookup currentRoomName . clientRooms)
    liftIO $ do
        putStrLn [qc|Users invited to room '{currentRoomName}'|]
        mapM_ putStrLn (invitedUsers currentRoom)
    return NoAction


inviteUserToRoom :: CommandsT StateM ()
inviteUserToRoom = custom "invite" "Invite to room" parseUser always $ \userName -> do
    currentRoomName <- fromJust <$> gets currentRoom
    invitor <- gets (inviteUserCLI . chatCommands)
    liftIO $ do
        putStrLn [qc|Inviting '{userName}'...|]
        invitor currentRoomName userName
    return NoAction
  where
    always = return True
    parseUser node input = do
        usersList <- map userName <$> (gets (getCurrentOnlineUsersCLI . chatCommands) >>= liftIO)
        parseOneOf usersList "whom to invite" node input


chatInRoom :: CommandsT StateM ()
chatInRoom = command "chat" "Chat in room" $ do
    currentRoomName <- fromJust <$> gets currentRoom
    runChatWindow' <- gets (runChatWindowCLI . chatCommands)
    liftIO $ do
        putStrLn $ "Start chatting in room '" ++ currentRoomName ++ "'..."
        runChatWindow' currentRoomName
        putStrLn $ "Chatting in room '" ++ currentRoomName ++ "' finished"
    return NoAction


-- * --------------------------------------------------------------------------


runChat :: UserName -> ChatCommandsEx -> ClientStateRef -> IO ()
runChat userName chatCommandsEx clientStateRef = do
    let settings = def { getBanner = "HasChat\nEnter '?' to list of commands\nUse 'Tab' to completion!"
                       , getHistory = Just ".haschat.history" }
        initialState = ClientStateEx clientStateRef Nothing chatCommandsEx userName
    flip evalStateT initialState $ do
        result <- runCLI "HasChat" settings root
        either (error . show) return result

