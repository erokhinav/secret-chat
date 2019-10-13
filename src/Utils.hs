module Utils where


import Control.Concurrent
import Foreign.StablePtr
import Network.Simple.TCP
import Network.Socket
import System.IO


sockToHandle :: Socket -> IO Handle
sockToHandle sock = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetNewlineMode handle (NewlineMode CRLF CRLF)
    return handle


-- | Stop the thread forever
waitForever :: IO ()
waitForever = do
    lock <- newEmptyMVar
    _ <- newStablePtr =<< myThreadId
    takeMVar lock
