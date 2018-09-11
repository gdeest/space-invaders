module Connector where

import Network.Socket

data Connection = Connection 
  { host :: String
  , port :: Integer
  , socket :: Network.Socket
  }

init :: Connection
init = Connection
  { host = "127.0.0.1"
  , port = 7171
  }

setHost :: String -> Connection
setHost str = Connection{ host = str }

getHost :: Connection -> String
getHost c = Connection.host

setPort :: Integer -> Connection
setPort port = Connection{ port = port }

getPort :: Connection -> Integer
getPort c = Connection.port

setSocket :: Connection -> Connection
setSocket c = do
  
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getADdrInfo (Just hints) (Just host) (Just port)
  return addr

open addr = do
  sock <- Network.sock (addrFamily addr) (addrSocketType addr) (addrProtocol port)
  connect sock $ addrAddress addr
  return sock
