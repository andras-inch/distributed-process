module Main where
import Network.Transport.TCP (createTransport, defaultTCPParameters, defaultTCPAddr)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
  Right t <- createTransport (defaultTCPAddr "127.0.0.1" "10501") defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  _ <- runProcess node $ do
    stats <- getLocalNodeStats
    liftIO $ print stats -- this works fine
    s <- getLocalNodeInfo
    liftIO $ putStrLn s -- this just hangs
  return ()