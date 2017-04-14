{-# LANGUAGE
    FlexibleContexts
  , PolyKinds
  #-}
{-|
Stability : experimental

Exports RoutingTableOp which encapsulates the operations we must know how to perform,
in order to maintain routing contacts.
 -}
module DHT.Op.RoutingTable
  (RoutingTableOp(..)
  ,RTInsertF
  ,RTLookupF
  ) where

import DHT.Contact
import DHT.ID
import DHT.Types

-- | Operations on a routing table
data RoutingTableOp dht m = RoutingTableOp
    { _routingTableOpInsert :: RTInsertF dht m -- ^ Insert a new 'Addr'ess
    , _routingTableOpLookup :: RTLookupF m -- ^ Lookup an 'Addr'ess
    , _routingTableOpKSize  :: m Int       -- ^ Query the bucket size/ how many neighbours we expect to get back
    }

-- | Insert an 'Addr'ess at a 'Time'. Ping Questionable Contacts with the given function if needed.
type RTInsertF dht m = Addr -> Time -> (Addr -> dht m Bool) -> dht m ()

-- | Lookup an 'Addr'ess and neighbouring Contacts to an 'ID' in 'm'.
type RTLookupF m = Addr -> ID -> Time -> m ([Contact],Maybe Contact)

