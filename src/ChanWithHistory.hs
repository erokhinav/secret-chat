-- | Chan with history that can aquire data and return it
--
{-# LANGUAGE RecordWildCards #-}
module ChanWithHistory
    ( ChanWithHistory
    , newChanWithHistory
    , newChanWithHistory'
    , getHistoryAndFetcher
    , push
    ) where


import Control.Concurrent.STM
import Control.Monad.Loops


data ChanWithHistory a = ChanWithHistory
        { chan :: TChan a
        , dup  :: TVar (TChan a)
        }


newChanWithHistory :: IO (ChanWithHistory a)
newChanWithHistory = newChanWithHistory' []


newChanWithHistory' :: [a] -> IO (ChanWithHistory a)
newChanWithHistory' lst = do
    chan <- newTChanIO
    atomically $ mapM_ (writeTChan chan) lst
    dup  <- atomically $ (cloneTChan chan >>= newTVar)
    return ChanWithHistory{..}


push :: ChanWithHistory a -> a -> IO ()
push ChanWithHistory{..} a = atomically $ writeTChan chan a


getHistoryAndFetcher :: ChanWithHistory a -> IO ([a], IO a)
getHistoryAndFetcher ChanWithHistory{..} =
    atomically $ do
        dup'  <- readTVar dup
        dup'' <- cloneTChan dup'
        writeTVar dup dup''
        history <- unfoldM (tryReadTChan dup')
        let reader = atomically $ readTChan dup'
        return (history, reader)

