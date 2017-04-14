{-|
Stability : experimental

Run a DHT computation using "DHT.SimpleNode.Logger", "DHT.SimpleNode.Messaging", "DHT.SimpleNode.RoutingTable"
and "DHT.SimpleNode.ValueStore" for stdout logging, simple UDP messaging a wrapped "DHT.Routing" routing table
and an in-memory hashmap value store.
-}
module DHT.SimpleNode
  ( newSimpleNode
  )
  where

import           Control.Applicative
import           Control.Concurrent
import           Data.Time.Clock.POSIX
import           Network.Socket                       (Socket,socket,Family(AF_INET),SocketType(Datagram),inet_addr,SockAddr(SockAddrInet),inet_ntoa,bind)
import           System.Random
import qualified Data.ByteString.Char8      as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Network.Socket.ByteString  as Strict

import DHT
import DHT.Contact
import DHT.ID
import DHT.Message
import DHT.Types

import DHT.SimpleNode.Logging
import DHT.SimpleNode.Messaging
import DHT.SimpleNode.RoutingTable
import DHT.SimpleNode.ValueStore

-- | Start a new node. Attempt to bind ourself to the given 'Addr'ess, bootstrap of a possible 'Addr'ess
-- , log with the given 'Logger' and execute the given 'DHT IO a' computation providing an error or
-- the successful result.
-- - Will handle incoming messages for the duration of the enclosing program.
-- Continuing communication after we reach the end of our own DHT computation must be programmed explicitly.
newSimpleNode :: Addr -> Maybe Addr -> LoggingOp IO -> DHT IO a -> IO (Either DHTError a)
newSimpleNode ourAddr mBootstrapAddr logging dht = do

  now          <- timeF
  routingTable <- newSimpleRoutingTable size ourID now
  valueStore   <- newSimpleValueStore
  messaging    <- newSimpleMessaging size (maxPortLength,ourPort)

  let run :: Maybe Addr -> DHT IO a -> IO (Either DHTError a)
      run = runDHT ourAddr size timeF randF messaging routingTable valueStore logging

  _ <- let handleMessages :: IO x
           handleMessages = do
               ourSock <- socket AF_INET Datagram 17

               let udpPort = fromInteger $ toInteger ourPort
               inetAddr <- inet_addr ourIP
               bind ourSock $ SockAddrInet udpPort inetAddr

               handleMessages' ourSock

           handleMessages' :: Socket -> IO b
           handleMessages' ourSock = do
               (msg,SockAddrInet _ fromHost) <- Strict.recvFrom ourSock 1024
               let (replyPortMsg,msg') = Strict.splitAt maxPortLength msg

               fromIP <- inet_ntoa fromHost
               let replyPort = toPort replyPortMsg
                   replyAddr = Addr fromIP replyPort

               case decodeSomeMessage (Lazy.fromStrict msg') of

                 -- Silently drop non-messages
                 Nothing
                   -> handleMessages' ourSock

                 Just (SomeMessage message)
                   -> do res <- run Nothing $ handleMessage replyAddr message
                         case res of

                           -- handling the message failed somehow. Continue silently.
                           Left _   -> handleMessages' ourSock
                           Right () -> handleMessages' ourSock
          in forkIO handleMessages

  run mBootstrapAddr dht
  where
    Addr ourIP ourPort = ourAddr
    ourID              = mkID ourAddr size

    size          = 8
    maxPortLength = 5

    -- Current time in seconds since epoch
    timeF :: IO Time
    timeF = round <$> getPOSIXTime

    randF :: IO Int
    randF = randomRIO (0,maxBound)

    toPort :: Strict.ByteString -> Port
    toPort = read . Strict.unpack

