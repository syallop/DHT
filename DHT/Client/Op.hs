{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-|
Stability : experimental

Exports 'Op' which encapsulates the operations we must know how to perform in order to implement a DHT.

 -}
module DHT.Client.Op
  ( Op()
  , mkOp

  -- * Use
  , currentTime
  , randomInt

  -- ** Messaging
  , Op.Messaging
  , Op.mkMessaging
  , withMessaging
  , wait
  , route
  , sendBytes
  , recvBytes

  -- ** Routing Table
  , Op.RoutingTable
  , Op.mkRoutingTable
  , withRoutingTable
  , insertAddress
  , lookupContact
  , kSize

  -- ** Value Store
  , Op.ValueStore
  , Op.mkValueStore
  , withValueStore
  , insertIDValue
  , lookupIDValue

  -- ** Logging
  , Logging
  , withLogging

  -- * Type synonyms
  , Op.SendF
  , Op.WaitF
  , Op.RouteF
  , Op.RecvF
  , Op.RTInsertF
  , Op.RTLookupF
  , Op.ValInsertF
  , Op.ValLookupF
  )
  where

import qualified DHT.Client.Op.Messaging    as Op
import qualified DHT.Client.Op.RoutingTable as Op
import qualified DHT.Client.Op.ValueStore   as Op

import DHT.Core.Types

-- | A Possible logging function in 'm'.
type Logging m = Maybe (String -> m ())

-- | The operations we require to implement a DHT
data Op dht m = Op
  { _opTime         :: m Time                -- ^ Determine the current time
  , _opRandomInt    :: m Int                 -- ^ Generate a random Int
  , _opMessaging    :: Op.Messaging m        -- ^ Operations for sending, waiting and routing messages
  , _opRoutingTable :: Op.RoutingTable dht m -- ^ Operations for tracking routing contacts
  , _opValueStore   :: Op.ValueStore m       -- ^ Operations for value storage and retrieval
  , _opLogging      :: Logging m             -- ^ Operations for logging output
  }

-- | Construct an 'Op', which describes concrete implementations of how to
-- perform the various operations required to support our DHT.
mkOp
  :: m Time                -- ^ Determine the current time
  -> m Int                 -- ^ Generate a random Int
  -> Op.Messaging m        -- ^ Send, wait and route messages
  -> Op.RoutingTable dht m -- ^ Track routing contacts
  -> Op.ValueStore m       -- ^ Store and retrieve values
  -> Logging m             -- ^ Log output
  -> Op dht m
mkOp = Op

-- | Determine the current time.
currentTime
  :: Op dht m
  -> m Time
currentTime = _opTime

-- | Generate a random Int.
randomInt
  :: Op dht m
  -> m Int
randomInt = _opRandomInt

-- | Apply a function to the Messaging operations.
withMessaging
  :: (Op.Messaging m -> m a)
  -> Op dht m
  -> m a
withMessaging f = f . _opMessaging

-- | Wait for a response to a sent message.
-- The sent message may be identified by it's 'Command c' and it's input 'In c'.
-- 'Out c' gives the response message.
wait
  :: Op dht m
  -> Op.WaitF m
wait = Op.wait . _opMessaging

-- | Route a received response to a waiter.
route
  :: Op dht m
  -> Op.RouteF m
route = Op.route . _opMessaging

-- | Physically send bytes that should encode a Command to an Address.
sendBytes
  :: Op dht m
  -> Op.SendF m
sendBytes = Op.sendBytes . _opMessaging

-- | Physically receiving bytes on an listener address.
recvBytes
  :: Op dht m
  -> Op.RecvF m
recvBytes = Op.recvBytes . _opMessaging

-- | Apply a function to the RoutingTable operations.
withRoutingTable
  :: (Op.RoutingTable dht m -> m a)
  -> Op dht m
  -> m a
withRoutingTable f = f . _opRoutingTable

-- | Insert a new 'Address' at a 'Time'. Ping 'Questionable' Contacts if needed.
insertAddress
  :: Op dht m
  -> Op.RTInsertF dht m
insertAddress = Op.insertAddress . _opRoutingTable

-- | Lookup an 'Address' and neighbouring 'Contacts' to an 'ID'.
lookupContact
  :: Op dht m
  -> Op.RTLookupF m
lookupContact = Op.lookupContact . _opRoutingTable

-- | Query the bucket size/ how many neighbours we expect to get back.
kSize
  :: Op dht m
  -> m Int
kSize = withRoutingTable Op.kSize

-- | Apply a function to the ValueStore operations.
withValueStore
  :: (Op.ValueStore m -> m a)
  -> Op dht m
  -> m a
withValueStore f = f . _opValueStore

-- | Insert a value into the storage.
insertIDValue
  :: Op dht m
  -> Op.ValInsertF m
insertIDValue = Op.insert . _opValueStore

-- | Lookup a value in the storage.
lookupIDValue
  :: Op dht m
  -> Op.ValLookupF m
lookupIDValue = Op.lookup . _opValueStore

-- | Apply a function to the Logging operations.
withLogging
  :: (Logging m -> m a)
  -> Op dht m
  -> m a
withLogging f = f . _opLogging

