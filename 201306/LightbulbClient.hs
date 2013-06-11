module LightBulbClient where

import Network.Socket
import Control.Monad (forever, void)

port = 9876
host = "127.0.0.1"

main = withSocketsDo $ do
    s <- socket AF_INET Datagram defaultProtocol
    setSocketOption s Broadcast 1
    hostAddr <- inet_addr host
    void $ forever $ do
        msg <- getLine
        sendTo s msg (SockAddrInet port hostAddr)
    sClose s
    return()
