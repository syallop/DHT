{-# LANGUAGE
    FlexibleContexts
  , RankNTypes
  #-}
{-|
Stability : experimental

Exports MessagingOp which encapsulates the operations we must know how to perform,
in order to send and recieve messages.
 -}
module DHT.Op.Messaging
  ( MessagingOp(..)
  , WaitF
  , RouteF
  , SendF
  , RecvF
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Typeable

import DHT.Address
import DHT.Command

-- | Messaging operations.
-- In some 'm' provide functions for:
data MessagingOp m = MessagingOp
  {_messagingOpWaitResponse  :: WaitF m  -- ^ Waiting for a response to a send message
  ,_messagingOpRouteResponse :: RouteF m -- ^ Routing a recieved response to a waiter

  ,_messagingOpSendBytes     :: SendF m  -- ^ Physically sendind bytes to an address
  ,_messagingOpRecvBytes     :: RecvF m  -- ^ Physically receive bytes on an address to listen on
  }

-- | Wait on the 'Out'put response in 'm' to a sent 'Command' with 'In'put.
type WaitF  m = forall c. Typeable (Out c) => Command c -> In c -> m (Out c)

-- | Route a 'Resp'onse to a 'Command' to a waiter in 'm'.
type RouteF m = forall c. (Typeable (Out c)) => Command c -> Resp c -> m ()

-- | Send bytes to an 'Address's in 'm'.
type SendF  m = Address -> ByteString -> m ()

-- | Receive Bytes on an listening address in 'm'.
-- Also return the sender.
type RecvF  m = Address -> m (Address, ByteString)

