module Database.PostgresInterface.Serve
  ( servePostgres
  , ServerConfig (..)
  , defaultConfig
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Database.PostgresInterface.Protocol.Server (runConnection)
import Database.PostgresInterface.Table (AnyTable)
import Network.Socket

data ServerConfig = ServerConfig
  { serverPort :: Int
  }
  deriving (Show)

defaultConfig :: ServerConfig
defaultConfig = ServerConfig { serverPort = 5432 }

-- | Start a PostgreSQL-compatible server, accepting connections in a loop.
servePostgres :: ServerConfig -> [AnyTable] -> IO ()
servePostgres cfg _tables =
  withSocketsDo $ do
    addr <- resolve (serverPort cfg)
    bracket (open addr) close acceptLoop
  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))

    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 1024
      pure sock

    acceptLoop sock = do
      (conn, _peer) <- accept sock
      _ <- forkIO (runConnection conn `finally` close conn)
      acceptLoop sock
