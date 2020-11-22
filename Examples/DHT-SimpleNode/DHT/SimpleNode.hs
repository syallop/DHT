{-|
Stability : experimental

Run a DHT computation using "DHT.SimpleNode.Logger", "DHT.SimpleNode.Messaging", "DHT.SimpleNode.RoutingTable"
and "DHT.SimpleNode.ValueStore" for stdout logging, simple UDP messaging a wrapped "DHT.Routing" routing table
and an in-memory hashmap value store.
-}
module DHT.SimpleNode
  ( mkSimpleNodeConfig
  , newSimpleNode
  )
  where

import           Control.Concurrent
import           Data.Time.Clock.POSIX
import           System.Random

import DHT
import DHT.Client
import DHT.Core
import qualified DHT.Client.Op as Op

import DHT.SimpleNode.Messaging
import DHT.SimpleNode.RoutingTable
import DHT.SimpleNode.ValueStore

import Control.Monad

mkSimpleNodeConfig
  :: Address
  -> Int
  -> Logging IO
  -> Maybe Address
  -> IO (Config DHT IO)
mkSimpleNodeConfig ourAddress hashSize logging mBootstrapAddress = do
  now          <- timeF
  routingTable <- newSimpleRoutingTable maxBucketSize ourID now hashSize
  valueStore   <- newSimpleValueStore
  messaging    <- newSimpleMessaging hashSize (maxPortLength, ourAddress)

  let ops = Op.mkOp timeF randF messaging routingTable valueStore logging
  pure $ mkConfig ops ourAddress hashSize mBootstrapAddress
  where
    timeF :: IO Time
    timeF = round <$> getPOSIXTime

    randF :: IO Int
    randF = randomRIO (0,maxBound)

    ourID = mkID ourAddress hashSize

    maxPortLength = 5

    maxBucketSize = 8

-- | Start a new node with some configuration.
-- - Will handle incoming messages for the duration of the given program.
-- Continuing communication after we reach the end of our own DHT computation must be programmed explicitly.
newSimpleNode :: Config DHT IO
              -> DHT IO a
              -> IO (Either DHTError a)
newSimpleNode config dht = do
  forkIO $ void $ startMessaging config
  runDHT config $ bootstrap >> dht

