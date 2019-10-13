{-# LANGUAGE RecordWildCards #-}
module Tests.DbSpec where


import Crypto.Number.Serialize
import Crypto.PubKey.DH
import Data.Time.Clock
import System.IO.Temp
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Test.Hspec

import ChanWithHistory
import ClientState
import Database
import Types


spec :: Spec
spec = describe "Database" $ do
    it "must save and restore state" $ example $ do
        now <- getCurrentTime
        let user1 = UserDescription "vasya" "v.123" 5010
            user2 = UserDescription "petya" "p.123" 5020
            user3 = UserDescription "kolya" "k.123" 5030
            user4 = UserDescription "ivan"  "i.123" 5040
            user5 = UserDescription "john"  "j.123" 5050
            user6 = UserDescription "ben"   "b.123" 5060
            history1 = [ ChatMessage user2 now "Сообщение X 1"
                       , ChatMessage user3 now "Сообщение X 2"
                       , ChatMessage user4 now "Сообщение X 3"
                       , ChatMessage user5 now "Сообщение X 4"
                       , ChatMessage user4 now "Сообщение X 5"
                       ]
            history2 = [ ChatMessage user1 now "Сообщение Z 1"
                       , ChatMessage user5 now "Сообщение Z 2"
                       , ChatMessage user6 now "Сообщение Z 3"
                       , ChatMessage user5 now "Сообщение Z 4"
                       , ChatMessage user6 now "Сообщение Z 5"
                       ]
            myRoom1 = ClientRoom { invitedUsers = Set.fromList $ map userName [user1, user2, user3, user4, user5]
                                 , clientRoomMessages = undefined
                                 , dhPrivNumber = undefined
                                 , dhSharedKey = Right $ SharedKey $  i2ospOf_ 10 1235678
                                 }
            myRoom2 = ClientRoom { invitedUsers = Set.fromList $ map userName [user1, user5, user6]
                                 , clientRoomMessages = undefined
                                 , dhPrivNumber = undefined
                                 , dhSharedKey = Right $ SharedKey $  i2ospOf_ 10 555666777
                                 }
        --
        withSystemTempFile "haschat-db" $ \tmpdb _ -> do
            --
            -- Write to DB some info...
            --
            withDb tmpdb $ \conn -> do
                saveRoom conn "myroom1" myRoom1
                saveRoom conn "myroom2" myRoom2
                mapM_ (saveMessage conn "myroom1") history1
            withDb tmpdb $ \conn -> do
                mapM_ (saveMessage conn "myroom2") history2
            --
            -- Now check it
            --
            withDb tmpdb $ \conn -> do
                loadState conn "userX" >>= \ClientState{..} -> do
                    (Map.size clientRooms) `shouldBe` 2
                    let myRoom1' = clientRooms Map.! "myroom1"
                    let myRoom2' = clientRooms Map.! "myroom2"
                    (Set.toList $ invitedUsers myRoom1') `shouldBe` ["ivan","john","kolya","petya","vasya"]
                    (Set.toList $ invitedUsers myRoom2') `shouldBe` ["ben","john","vasya"]
                    (history1', _) <- getHistoryAndFetcher (clientRoomMessages myRoom1')
                    history1' `shouldBe` history1
                    (history2', _) <- getHistoryAndFetcher (clientRoomMessages myRoom2')
                    history2' `shouldBe` history2
                    dhSharedKey myRoom1' `shouldBe` dhSharedKey myRoom1
                    dhSharedKey myRoom2' `shouldBe` dhSharedKey myRoom2
