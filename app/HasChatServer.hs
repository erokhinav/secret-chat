-- | Server CLI
--
module Main where


import Control.Monad
import System.Environment
import System.Exit

import Server
import Logging


main :: IO ()
main = withLogging True "haschat-server.log" $ do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Run:"
        putStrLn "haschat-server <server-port>"
        putStrLn "Example:"
        putStrLn "haschat-server 64242"
        exitFailure
    let port = read $ head args
    info "Starting server"
    Server.run port
