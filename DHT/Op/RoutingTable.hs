{-# LANGUAGE
    FlexibleContexts
  , PolyKinds
  #-}
{-|
Stability : experimental

Exports RoutingTable which encapsulates the operations we must know how to perform,
in order to maintain routing contacts.
 -}
module DHT.Op.RoutingTable
  ( RoutingTable()
  , mkRoutingTable

  , RTInsertF
  , RTLookupF

  , insertAddress
  , lookupContact
  , kSize
  ) where

import DHT.Address
import DHT.Contact
import DHT.ID
import DHT.Types

-- TODO:
-- - Ping function should standalone?
-- - Should bucket size and number of neighbours be the same?

-- | Operations on a routing table
data RoutingTable dht m = RoutingTable
  { _routingTableInsert :: RTInsertF dht m -- ^ Insert a new 'Addr'ess
  , _routingTableLookup :: RTLookupF m     -- ^ Lookup an 'Addr'ess
  , _routingTableKSize  :: m Int           -- ^ Query the bucket size/ how many neighbours we expect to get back
  }

-- | Construct a RoutingTable by providing operations for inserting Addresses,
-- looking up Contacts and returning 'k' - the bucket size/ amount of neighbours
-- expected in a lookup.
mkRoutingTable
  :: RTInsertF dht m -- ^ Insert a new 'Address'.
  -> RTLookupF m     -- ^ Lookup a Contact associated with an 'ID'.
  -> m Int           -- ^ The bucket size/ how many neighbours expected in a lookup.
  -> RoutingTable dht m
mkRoutingTable = RoutingTable

-- | Insert an 'Address' at a 'Time'. Ping Questionable Contacts with the given function if needed.
type RTInsertF dht m = Address -> Time -> (Address -> dht m Bool) -> dht m ()

-- | Lookup an 'Address' and neighbouring Contacts to an 'ID' in 'm'.
type RTLookupF m = Address -> ID -> Time -> m ([Contact], Maybe Contact)

-- | Insert a new 'Address' at a 'Time'. Ping 'Questionable' Contacts if needed.
insertAddress
  :: RoutingTable dht m
  -> RTInsertF dht m
insertAddress = _routingTableInsert

-- | Lookup an 'Address' and neighbouring 'Contacts' to an 'ID'.
lookupContact
  :: RoutingTable dht m
  -> RTLookupF m
lookupContact = _routingTableLookup

-- | Query the bucket size/ how many neighbours we expect to get back.
kSize
  :: RoutingTable dht m
  -> m Int
kSize = _routingTableKSize

