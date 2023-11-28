{-|
Stability : experimental

Wraps DHT.Routing into a DHT RoutingTable using MVar shared state.
 -}
module DHT.SimpleNode.RoutingTable
  (newSimpleRoutingTable
  )
  where

import Prelude hiding (lookup)

import DHT
import DHT.Client
import DHT.Core

import Control.Concurrent

type RTState = MVar Routing

-- Insert a new address into the routingtable, pinging questionable nodes with
-- the ping function to update them if required.
rtInsert :: RTState -> Address -> Time -> (Address -> DHT IO Bool) -> DHT IO ()
rtInsert rtState address time pingAddress = do
  rt  <- liftDHT $ takeMVar rtState

  size <- hashSize

  rt' <- insert address time pingAddress size rt
  liftDHT $ putMVar rtState rt'

-- Lookup the Contact associated with an 'ID', also return k neighbour contacts
-- relative from the enquiring Addr.
rtLookup :: RTState -> Address -> ID -> Time -> Int -> IO ([Contact],Maybe Contact)
rtLookup rtState enquirerAddr targetID now commonHashSize = do
  rt <- takeMVar rtState
  let (rt',res) = lookup enquirerAddr targetID now commonHashSize rt
  putMVar rtState rt'
  return res

newSimpleRoutingTable :: Int -> ID -> Time -> Int -> IO (RoutingTable DHT IO)
newSimpleRoutingTable size ourCID now commonHashSize = do
  rtState <- newMVar $ empty size ourCID now
  pure $ mkRoutingTable
    (rtInsert rtState)
    (\addr enquirer time
      -> rtLookup rtState addr enquirer time commonHashSize)
    (pure size)

