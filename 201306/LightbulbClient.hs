module LightBulbClient where

import Data.Foldable (forM_)
import Network.Socket
import Control.Monad (forever, void)

port = 9876
host = "127.0.0.1"

bulbHosts :: [String]
bulbHosts = ["141.228.108.156", "141.228.108.152"]

main :: IO ()
main = withSocketsDo $ do
    s <- socket AF_INET Datagram defaultProtocol
    bulbs <- mapM (fmap (SockAddrInet port) . inet_addr) bulbHosts
    setSocketOption s Broadcast 1
    void $ forever $ do
        msg <- getLine
        forM_ bulbs $ sendTo s msg
    sClose s
    return ()
