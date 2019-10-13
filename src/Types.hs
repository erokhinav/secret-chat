module Types where


import Network.Simple.TCP
import Network.Socket
import Data.Time.Clock


type UserName = String


type RoomName = String


type Time = UTCTime


data UserDescription = UserDescription
        { userName      :: UserName
        , userHost      :: HostName
        , userPort      :: PortNumber
        }
        deriving (Read, Show, Eq)


data ChatMessage = ChatMessage
        { user    :: UserDescription
        , time    :: UTCTime
        , message :: String
        }
        deriving (Read, Show, Eq)


type History = [ChatMessage]


type RunChatWindow = RoomName -> IO ()

