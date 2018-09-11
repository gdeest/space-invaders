module Connector where

import Network
import Network.Socket
import Network.Socket.ByteString
  
data Connection = Connection 
  { host :: String
  , port :: String
  , socket :: Network.Socket
  }

setHost :: Connection -> String -> Connection
setHost c h = c{ host = h }

getHost :: Connection -> String
getHost Connection{ host = h} = h

setPort :: Connection -> String -> Connection
setPort c p = c{ port = p }

getPort :: Connection -> String
getPort Connection{ port = p} = p

initConnection Connection{ host = h, port = p } = do
  let hints = defaultHints { Network.Socket.addrSocketType = Stream }
  addr:_ <- Network.Socket.getAddrInfo (Just hints) (Just h) (Just p)
  sock <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  return Connection{ Connector.host = h
                   , Connector.port = p
                   , Connector.socket = sock }

sendData Connection{ Connector.socket = sock } d = do
  Network.Socket.ByteString.sendAll sock d
  return ()

recvData Connection{ Connector.socket = sock } = do
  return $ Network.Socket.ByteString.recv sock 1024
