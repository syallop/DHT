{-|
Stability : experimental

Wraps DHT.Routing into a DHT RoutingTable using MVar shared state.
 -}
module DHT.SimpleNode.RoutingTable
  (newSimpleRoutingTable
  )
  where

import Prelude hiding (insert,lookup)

import DHT
import DHT.Contact
import DHT.ID
import DHT.Routing
import DHT.Types

import Control.Concurrent

type RTState = MVar Routing

-- Insert a new address into the routingtable, pinging questionable nodes with
-- the ping function to update them if required.
rtInsert :: RTState -> Addr -> Time -> (Addr -> DHT IO Bool) -> DHT IO ()
rtInsert rtState addr time ping = do
  rt  <- liftDHT $ takeMVar rtState

  hashSize <- askHashSize

  rt' <- insert addr time ping hashSize rt
  liftDHT $ putMVar rtState rt'

-- Lookup the Contact associated with an 'ID', also return k neighbour contacts
-- relative from the enquiring Addr.
rtLookup :: RTState -> Addr -> ID -> Time -> Int -> IO ([Contact],Maybe Contact)
rtLookup rtState enquirerAddr targetID now hashSize = do
  rt <- takeMVar rtState
  let (rt',res) = lookup enquirerAddr targetID now hashSize rt
  putMVar rtState rt'
  return res

newSimpleRoutingTable :: Int -> ID -> Time -> Int -> IO (RoutingTableOp DHT IO)
newSimpleRoutingTable size ourID now hashSize = mkRoutingTable <$> newRTState
  where
    mkRoutingTable rtState = RoutingTableOp (rtInsert rtState)
                                            (\addr enquirer time
                                              -> rtLookup rtState addr enquirer time hashSize)
                                            (pure size)
    newRTState = newMVar $ empty size ourID now

