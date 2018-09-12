module Connector where

import Network
import Network.Socket
import Network.Socket.ByteString
import Data.BERT
import Data.ByteString
import Data.Binary
import Network.BERT
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8
  
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

init =
  initConnection Connection{ host = "localhost", port = "7777" }

initConnection Connection{ host = h, port = p } = do
  let hints = defaultHints { Network.Socket.addrSocketType = Stream }
  addr:_ <- Network.Socket.getAddrInfo (Just hints) (Just h) (Just p)
  sock <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  return Connection{ Connector.host = h
                   , Connector.port = p
                   , Connector.socket = sock }

toBert d = Data.ByteString.Lazy.toStrict $ Data.Binary.encode . Data.BERT.showBERT $ d

fromBert d = Data.ByteString.Lazy.Char8.toStrict $ Data.Binary.decode $ d

sendData Connection{ Connector.socket = sock } d = do
    Network.Socket.ByteString.sendAll sock $ toBert d
    return ()

recvData Connection{ Connector.socket = sock } = do
  return $  Network.Socket.ByteString.recv sock 1024
