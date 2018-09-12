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
  
init :: IO Network.Socket
init = initConnection "localhost" "7777"

initConnection :: String -> String -> IO Network.Socket
initConnection host port = do
  let hints = defaultHints { Network.Socket.addrSocketType = Stream }
  addr:_ <- Network.Socket.getAddrInfo (Just hints) (Just host) (Just port)
  s <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect s $ Network.Socket.addrAddress addr
  return s

toBert d = Data.ByteString.Lazy.toStrict $ Data.Binary.encode . Data.BERT.showBERT $ d

fromBert d = Data.ByteString.Lazy.Char8.toStrict $ Data.Binary.decode $ d

sendData sock d = Network.Socket.ByteString.sendAll sock $ toBert d

recvData sock = Network.Socket.ByteString.recv sock 1024
