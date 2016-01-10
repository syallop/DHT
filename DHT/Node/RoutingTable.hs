{-|
Stability : experimental

Wraps DHT.Routing into a DHT RoutingTable using MVar shared state.
 -}
module DHT.Node.RoutingTable
  (newRoutingTable
  )
  where

import Prelude hiding (insert,lookup)

import DHT
import DHT.Contact
import DHT.ID
import DHT.Routing
import DHT.Types

import Control.Applicative hiding (empty)
import Control.Concurrent

type RTState = MVar Routing

-- Insert a new address into the routingtable, pinging questionable nodes with
-- the ping function to update them if required.
rtInsert :: RTState -> Addr -> Time -> (Addr -> DHT IO Bool) -> DHT IO ()
rtInsert rtState addr time ping = do
  rt  <- liftDHT $ takeMVar rtState
  rt' <- insert addr time ping rt
  liftDHT $ putMVar rtState rt'

-- Lookup the Contact associated with an 'ID', also return k neighbour contacts
-- relative from the enquiring Addr.
rtLookup :: RTState -> Addr -> ID -> Time -> IO ([Contact],Maybe Contact)
rtLookup rtState enquirerAddr targetID now = do
  rt <- takeMVar rtState
  let (rt',res) = lookup enquirerAddr targetID now rt
  putMVar rtState rt'
  return res

newRoutingTable :: Int -> ID -> Time -> IO (RoutingTable IO)
newRoutingTable size ourID now = mkRoutingTable <$> newRTState
  where
    mkRoutingTable rtState = RoutingTable (rtInsert rtState) (rtLookup rtState) (pure size)
    newRTState = newMVar $ empty size ourID now

