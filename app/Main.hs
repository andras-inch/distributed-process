module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Map (keys)
import Network.Transport.TCP
  ( createTransport
  , defaultTCPAddr
  , defaultTCPParameters
  )
-- import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
               [] -> defaultPort
               [p] -> read p
               _ -> error "???"
  runLocalNode port $ do
     -- getSelfPid >>= say . show
     -- liftIO $ hFlush stderr
     stats <- getLocalNodeStats
     liftIO $ print stats
     liftIO . print =<< registered

registered :: Process [String]
registered = keys <$> getLocalRegistry

defaultPort :: Int
defaultPort = 9999

runLocalNode :: Int -> Process () -> IO ()
runLocalNode port action = do
  Right t <- createTransport
        (defaultTCPAddr "127.0.0.1" $ show port)
        defaultTCPParameters
  n <- newLocalNode t initRemoteTable
  runProcess n action
