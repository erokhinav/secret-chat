{-# LANGUAGE TemplateHaskell #-}
module Logging
    ( withLogging
    , info
    , debug
    , warn
    ) where


import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.Time.Format
import Prelude hiding (log)
import System.IO
import System.IO.Unsafe (unsafePerformIO)


logFileHandleRef :: IORef (Handle, Bool)
logFileHandleRef = unsafePerformIO $ newIORef (stderr, False)
{-# NOINLINE logFileHandleRef #-}


withLogging :: Bool -> FilePath -> IO () -> IO ()
withLogging isLogToStderr logFileName action = bracket startLogging finishLogging $ const action
  where
    startLogging = do
        logHandle <- openFile logFileName AppendMode
        writeIORef logFileHandleRef (logHandle, isLogToStderr)
        return logHandle
    finishLogging logHandle = do
        writeIORef logFileHandleRef (stderr, False)
        hClose logHandle


info :: String -> IO ()
info = log "INFO "


debug :: String -> IO ()
debug = log "DEBUG"


warn :: String -> IO ()
warn = log "WARN "


log :: String -> String -> IO ()
log prefix message = do
    (logHandle, isLogToStderr) <- readIORef logFileHandleRef
    now <- getCurrentTime
    let nowStr = formatTime defaultTimeLocale "%F %T" now
    let logMsg = "[" ++ prefix ++ "  " ++ nowStr ++ "] " ++ message
    hPutStrLn logHandle logMsg
    hFlush logHandle
    when isLogToStderr $ do
        hPutStrLn stderr logMsg
        hFlush stderr


