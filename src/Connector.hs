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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan


data Process = Process { sock :: IO Network.Socket
                       , input :: STM (TChan String)
                       , output :: STM (TChan String)
                       }

createState = 
  Process { sock = initConnection "localhost" "7779"
          , input = newTChan
          , output = newTChan
          }

createProcess = forkIO $ process $ createState

process state = do
  let Process { sock = s, input = i, output = o } = state
  ss <- s
  _ <- sendData ss "test"
  Prelude.putStrLn $ show ss
  process state

processRead input = do
  i <- input
  -- _ <- readTChan i
  return $ Prelude.putStr "read"

processWrite output = do
  o <- output
  -- _ <- writeTChan o
  return $ Prelude.putStr "write"


init :: IO Network.Socket
init = initConnection "localhost" "7777"

initConnection :: String -> String -> IO Network.Socket
initConnection host port = do
  let hints = defaultHints { Network.Socket.addrSocketType = Stream }
  addr:_ <- Network.Socket.getAddrInfo (Just hints) (Just host) (Just port)
  s <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  -- Network.Socket.setSocketOption s KeepAlive 1
  connect s $ Network.Socket.addrAddress addr
  return s

toBert d = Data.ByteString.Lazy.toStrict $ Data.Binary.encode . Data.BERT.showBERT $ d

fromBert d = Data.ByteString.Lazy.Char8.toStrict $ Data.Binary.decode $ d

sendData sock d = Network.Socket.ByteString.send sock $ toBert d

recvData sock = Network.Socket.ByteString.recv sock 1024
