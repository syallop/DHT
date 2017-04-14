{-|
Stability : experimental

Exports DHTOp which encapsulates the operations we must know how to perform,
in order to implement a DHT.
 -}
module DHT.Op
  (module Op
  ,LoggingOp
  ,DHTOp(..)
  )
  where

import DHT.Op.Messaging    as Op
import DHT.Op.RoutingTable as Op
import DHT.Op.ValueStore   as Op

import DHT.Types

-- | A Possible logging function in 'm'.
type LoggingOp m = Maybe (String -> m ())

-- | The operations we require to implement a DHT
data DHTOp dht m = DHTOp
  {_dhtOpTimeOp         :: m Time               -- ^ Determine the current time
  ,_dhtOpRandomIntOp    :: m Int                -- ^ Generate a random Int
  ,_dhtOpMessagingOp    :: MessagingOp m        -- ^ Operations for sending, waiting and routing messages
  ,_dhtOpRoutingTableOp :: RoutingTableOp dht m -- ^ Operations for tracking routing contacts
  ,_dhtOpValueStoreOp   :: ValueStoreOp m       -- ^ Operations for value storage and retrieval
  ,_dhtOpLoggingOp      :: LoggingOp m          -- ^ Operations for logging output
  }

