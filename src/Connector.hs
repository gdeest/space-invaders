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
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import GHC.IO.Handle
import GHC.IO.IOMode

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

-- This function return an handler instead of a socket
testConn host port = do
  let hints = defaultHints { Network.Socket.addrSocketType = Stream }
  addr:_ <- Network.Socket.getAddrInfo (Just hints) (Just host) (Just port)
  s <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect s $ Network.Socket.addrAddress addr
  h <- Network.Socket.socketToHandle s ReadWriteMode
  hSetBuffering h NoBuffering
  GHC.IO.Handle.hPutStr h "test"
  return h 

-- main data structure used: one thread to read and another
-- one to write data. We also share STM queue
data Container = Container { readThread :: ThreadId
                           , writeThread :: ThreadId
                           , readQueue :: STM( TQueue String)
                           , writeQueue :: STM (TQueue String)
                           }

-- create a connection and return container
-- data structure
createClientConnection host port = do
  createContainer $ testConn host port

-- create a container with thread initialization
createContainer :: IO Handle -> IO Container
createContainer handler = do
  let rq = newTQueue
  let wq = newTQueue
  h <- handler
  rt <- forkIO $ testReceive h rq
  wt <- forkIO $ testSend h wq
  return $ Container { readThread = rt
                     , writeThread = wt
                     , readQueue = rq
                     , writeQueue = wq
                     }
    
-- read message from queue and send it to handler
testSend handler queue = do
  q <- atomically $ queue
  message <- atomically $ readTQueue q
  GHC.IO.Handle.hPutStr handler message
  testSend handler queue

-- read message from handler and write it to the queue
testReceive handler queue = do
  q <- atomically $ queue
  message <- GHC.IO.Handle.hGetLine handler
  Prelude.putStrLn message
  atomically $ writeTQueue q message
  testReceive handler queue

-- send test function
s Container { writeQueue = wq } str = do
  q <- atomically wq
  atomically $ writeTQueue q str
  

toBert d = Data.ByteString.Lazy.toStrict $ Data.Binary.encode . Data.BERT.showBERT $ d

fromBert d = Data.ByteString.Lazy.Char8.toStrict $ Data.Binary.decode $ d

sendData sock d = Network.Socket.ByteString.send sock $ toBert d

recvData sock = Network.Socket.ByteString.recv sock 1024
