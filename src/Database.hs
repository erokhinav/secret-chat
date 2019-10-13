{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
module Database
    ( withDb
    , saveMessage
    , saveRoom
    , loadState
    , Connection
    , debug_storeAllMessages
    )
    where


import           Control.Exception
import           Control.Monad
import           Crypto.PubKey.DH
import           Data.ByteArray.Encoding
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Clock
import           Database.Beam hiding (time)
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Types
import ClientState


-- User -----------------------------------------------------------------------

data UserRecordT f = UserRecord
    { _userName :: Columnar f Text
    , _userHost :: Columnar f Text
    , _userPort :: Columnar f Int
    }
    deriving (Generic, Beamable)


instance Table UserRecordT where
    data PrimaryKey UserRecordT f = UserId (C f Text) deriving (Generic, Beamable)
    primaryKey = UserId . _userName

unUserId :: PrimaryKey UserRecordT f -> Columnar f Text
unUserId (UserId r) = r

deriving instance Show (PrimaryKey UserRecordT Identity)

type DbUser = UserRecordT Identity

type UserRecord = UserRecordT Identity

deriving instance Show UserRecord
deriving instance Eq UserRecord


-- Room -----------------------------------------------------------------------

data RoomT f = Room
    { _roomName  :: Columnar f Text
    , _roomKey   :: Columnar f Text
    , _roomUsers :: Columnar f Text
    }
    deriving (Generic, Beamable)


instance Table RoomT where
    data PrimaryKey RoomT f = RoomId (C f Text) deriving (Generic, Beamable)
    primaryKey = RoomId . _roomName

unRoomId :: PrimaryKey RoomT f -> Columnar f Text
unRoomId (RoomId r) = r

deriving instance Show (PrimaryKey RoomT Identity)


type DbRoom = RoomT Identity


-- Message --------------------------------------------------------------------


data MessageT f = Message
    { _messageId   :: Columnar f Int
    , _messageUser :: PrimaryKey UserRecordT f
    , _messageTime :: Columnar f UTCTime
    , _messageText :: Columnar f Text
    , _messageRoom :: PrimaryKey RoomT f
    } deriving (Generic, Beamable)

type DbMessage = MessageT Identity


instance Table MessageT where
    data PrimaryKey MessageT f = MessageId (C f Int) deriving (Generic, Beamable)
    primaryKey = MessageId . _messageId


deriving instance Show DbMessage
deriving instance Show (PrimaryKey MessageT Identity)

-- *DB* -----------------------------------------------------------------------


data ChatDb f = ChatDb
    { _chatMessages :: f (TableEntity MessageT)
    , _chatUsers    :: f (TableEntity UserRecordT)
    , _chatRooms    :: f (TableEntity RoomT) }
    deriving (Generic, Database be)


chatDb :: DatabaseSettings be ChatDb
chatDb = defaultDbSettings


-- * --------------------------------------------------------------------------

withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb fn act = withConnection fn $ \conn -> do
    createDb conn
    act conn


createDb :: Connection -> IO ()
createDb conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS messages (id INT NOT NULL, user__name VARCHAR NOT NULL, time TIMESTAMP NOT NULL, text VARCHAR NOT NULL, room__name VARCHAR NOT NULL, PRIMARY KEY(id));"
    execute_ conn "CREATE TABLE IF NOT EXISTS users (name VARCHAR NOT NULL, host VARCHAR NOT NULL, port INT NOT NULL, PRIMARY KEY(name));"
    execute_ conn "CREATE TABLE IF NOT EXISTS rooms (name VARCHAR NOT NULL, key VARCHAR NOT NULL, users VARCHAR NOT NULL, PRIMARY KEY(name));"


convertMsg :: RoomName -> ChatMessage -> Int -> DbMessage
convertMsg myRoomName ChatMessage{..} msgId =
    Message { _messageId = msgId
            , _messageUser = pk $ convertUser user
            , _messageTime = time
            , _messageText = T.pack message
            , _messageRoom = RoomId (T.pack myRoomName)
            }


convertUser :: UserDescription -> DbUser
convertUser UserDescription{..} =
    UserRecord { _userName = T.pack userName
               , _userHost = T.pack userHost
               , _userPort = fromIntegral userPort }


debug_storeAllMessages :: Connection -> RoomName -> History -> IO ()
debug_storeAllMessages conn roomName history = do
    -- createDb conn
    execute_ conn "DELETE FROM messages;"
    execute_ conn "DELETE FROM users;"
    liftIO $ runBeamSqlite conn $ do
        let (msgs, usrs) = convertHistory roomName history
        runInsert $ insert (_chatMessages chatDb) $ insertValues msgs
        runInsert $ insert (_chatUsers chatDb) $ insertValues usrs
    return ()
  where
    convertHistory :: RoomName -> History -> ([DbMessage], [DbUser])
    convertHistory myRoomName myHistory = (messages, users)
      where messages = zipWith (convertMsg myRoomName) myHistory [1..]
            users = map convertUser $ L.nubBy (\a b -> userName a == userName b) $ map user myHistory


saveMessage :: Connection -> RoomName -> ChatMessage -> IO ()
saveMessage conn roomName msg = do
    msgId <- (
        query_ conn "SELECT MAX(id) + 1 FROM messages" >>= \case
            [] -> return 1
            (Only mid:_) -> return mid) `catch` (\(_::SomeException) -> return 1)
    liftIO $ runBeamSqlite conn $ do
        runInsert $ insert (_chatMessages chatDb) $ insertValues [convertMsg roomName msg msgId]
        runSelectReturningOne (lookup_ (_chatUsers chatDb) (UserId $ T.pack $ userName $ user $ msg)) >>= \case
            Nothing ->  runInsert $ insert (_chatUsers chatDb) $ insertValues [convertUser $ user msg]
            (Just _) -> runUpdate $ save   (_chatUsers chatDb) (convertUser $ user msg)


saveRoom :: Connection -> RoomName -> ClientRoom -> IO ()
saveRoom conn roomName room =
    liftIO $ runBeamSqlite conn $ do
        runSelectReturningOne (lookup_ (_chatRooms chatDb) (RoomId $ T.pack $ roomName)) >>= \case
            Nothing ->  runInsert $ insert (_chatRooms chatDb) $ insertValues [convertRoom roomName room]
            (Just _) -> runUpdate $ save   (_chatRooms chatDb) (convertRoom roomName room)
  where
    convertRoom :: RoomName -> ClientRoom -> DbRoom
    convertRoom myRoomName ClientRoom{..} =
        Room { _roomName  = T.pack myRoomName
             , _roomKey   = either (const T.empty) (T.decodeUtf8 . convertToBase Base64) dhSharedKey
             , _roomUsers = T.pack $ show $ Set.toList invitedUsers
             }


loadState :: Connection -> UserName -> IO ClientState
loadState conn userName = do
    -- Loading data
    (dbRooms, dbMessages, dbUsers) <- liftIO $ runBeamSqlite conn $ do
        (,,) <$> (runSelectReturningList $ select $ all_ (_chatRooms chatDb))
             <*> (runSelectReturningList $ select $ all_ (_chatMessages chatDb))
             <*> (runSelectReturningList $ select $ all_ (_chatUsers chatDb))
    -- Convert to more compact
    let (users :: Map UserName UserDescription) = Map.fromList
              $ map (\UserRecord {..} ->
                  let un = T.unpack _userName
                  in (un, UserDescription { userName = un
                                          , userHost = T.unpack _userHost
                                          , userPort = fromIntegral _userPort}))
              $ dbUsers
    let unkUser = \myUserName -> UserDescription myUserName "?" 5000
    let (historyByRoom :: Map Text History) = Map.fromList
                       $ map (\g@((rn, _):_) -> (rn, map snd g))
                       $ L.groupBy (\(a, _) (b, _) -> a == b)
                       $ map (\Message{..} ->
                           let userId = T.unpack $ unUserId _messageUser
                           in (unRoomId _messageRoom, ChatMessage { user = fromMaybe (unkUser userId) $ Map.lookup userId users
                                                                  , time = _messageTime
                                                                  , message = T.unpack _messageText
                                                                  }))
                       $ dbMessages
    rooms <- fmap Map.fromList $ forM dbRooms $ \Room{..} -> do
        room' <- initRoomState' (Set.fromList $ read $ T.unpack _roomUsers)
                                (fromMaybe [] $ Map.lookup _roomName historyByRoom)
        let room = room' { dhSharedKey = either (\err -> error $ "Can't convert shared key: " ++ err)
                                                (Right . SharedKey)
                                                (convertFromBase Base64 $ T.encodeUtf8 _roomKey) }
        return (T.unpack _roomName, room)
    return $ ClientState rooms userName

