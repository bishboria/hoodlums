module LightBulb where

import Network.Socket
import Control.Monad (forever, when)

port = 9876
host = "0.0.0.0"

type Host = SockAddr

main :: IO ()
main = withSocketsDo $ do
        s <- socket AF_INET Datagram defaultProtocol
        bindAddr <- inet_addr host
        bindSocket s (SockAddrInet port bindAddr)
        forever $ do
                (msg, _, hostAddr) <- recvFrom s 1024
                putStrLn $ (show hostAddr) ++ ": " ++ msg

sendToAll :: Socket -> String -> [Host] -> IO ()
sendToAll socket msg hosts = do
        mapM_ (sendTo socket msg) $ hosts
