module ClientState where


import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import ChanWithHistory
import Crypto
import Types


data ClientState = ClientState
        { clientRooms   :: Map RoomName ClientRoom
        , clientName    :: UserName
        }


data ClientRoom = ClientRoom
        { invitedUsers       :: Set UserName
        , clientRoomMessages :: ChanWithHistory ChatMessage
        , dhPrivNumber       :: PrivateNumber -- ^ Client 'a' parameter; random number from [0..p]
        , dhSharedKey        :: Either SharedKey SharedKey  -- ^ Result of DH key exchange; initially (-1).
                                                            -- `Left` means key generated, but waiting for all client finish generating;
                                                            -- `Right` means all client have keys.
        }


type ClientStateRef = MVar ClientState


initRoomState :: UserName -> IO ClientRoom
initRoomState me = initRoomState' (Set.singleton me) []


initRoomState' :: Set UserName -> History -> IO ClientRoom
initRoomState' roomUsers history =
    ClientRoom roomUsers
        <$> newChanWithHistory' history
        <*> getRandomPrivateNumber
        <*> return (Left Crypto.emptySharedKey)

