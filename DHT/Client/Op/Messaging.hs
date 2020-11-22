{-# LANGUAGE
    FlexibleContexts
  , RankNTypes
  #-}
{-|
Stability : experimental

Exports Messaging which encapsulates the operations we must know how to perform,
in order to send and recieve messages.
 -}
module DHT.Client.Op.Messaging
  ( Messaging ()
  , mkMessaging

  , WaitF
  , RouteF
  , SendF
  , RecvF

  , wait
  , route
  , sendBytes
  , recvBytes
  ) where

import DHT.Core.Address
import DHT.Protocol.Command

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Typeable

-- | Messaging operations.
-- In some 'm' provide functions for:
data Messaging m = Messaging
  {_messagingWaitResponse  :: WaitF m  -- ^ Waiting for a response to a sent message
  ,_messagingRouteResponse :: RouteF m -- ^ Routing a received response to a waiter

  ,_messagingSendBytes     :: SendF m  -- ^ Physically sending bytes to an address
  ,_messagingRecvBytes     :: RecvF m  -- ^ Physically receive bytes on an address to listen on
  }

-- | Construct a Messaging by providing operations for:
-- - Sending and receiving bytes that should represent a Command
-- - Waiting and routing of received responses to senders.
mkMessaging
  :: WaitF m  -- ^ Waiting for responses to sent messages
  -> RouteF m -- ^ Routing a received response to a waiter
  -> SendF m  -- ^ Physically sending bytes to an address
  -> RecvF m  -- ^ Physically receiving bytes from a listened address
  -> Messaging m
mkMessaging = Messaging

-- | Wait on the 'Out'put response in 'm' to a sent 'Command' with 'In'put.
type WaitF  m = forall c. Typeable (Out c) => Command c -> In c -> m (Out c)

-- | Route a 'Resp'onse to a 'Command' to a waiter in 'm'.
type RouteF m = forall c. (Typeable (Out c)) => Command c -> Resp c -> m ()

-- | Send bytes to an 'Address's in 'm'.
type SendF  m = Address -> ByteString -> m ()

-- | Receive Bytes on an listening address in 'm'.
-- Also return the sender.
type RecvF  m = Address -> m (Address, ByteString)

-- | Wait for a response to a sent message.
-- The sent message may be identified by it's 'Command c' and it's input 'In c'.
-- 'Out c' gives the response message.
wait
  :: Messaging m
  -> WaitF m
wait = _messagingWaitResponse

-- | Route a received response to a waiter.
route
  :: Messaging m
  -> RouteF m
route = _messagingRouteResponse

-- | Physically send bytes that should encode a Command to an Address.
sendBytes
  :: Messaging m
  -> SendF m
sendBytes = _messagingSendBytes

-- | Physically receiving bytes on an listener address.
recvBytes
  :: Messaging m
  -> RecvF m
recvBytes = _messagingRecvBytes

