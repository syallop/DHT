{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  #-}
{-|
Stability : experimental

Exports MessagingOp which encapsulates the operations we must know how to perform,
in order to send and recieve messages.
 -}
module DHT.Op.Messaging
  (MessagingOp(..)
  ,WaitF
  ,RouteF
  ,SendF
  ) where

import Control.Applicative
import Control.Arrow              (first)
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List                  ((\\),nub)
import Data.Typeable

import DHT.Bucket
import DHT.Command
import DHT.Contact
import DHT.ID
import DHT.Message
import DHT.Types

-- | Messaging operations.
-- In some 'm' provide functions for:
data MessagingOp m = MessagingOp
  {_messagingOpWaitResponse  :: WaitF m  -- ^ Waiting for a response to a send message
  ,_messagingOpRouteResponse :: RouteF m -- ^ Routing a recieved response to a waiter

  ,_messagingOpSendBytes     :: SendF m  -- ^ Physically sendind bytes to an address
  }

-- | Wait on the 'Out'put response in 'm' to a sent 'Command' with 'In'put.
type WaitF  m = forall c. Typeable (Out c) => Command c -> In c -> m (Out c)

-- | Route a 'Resp'onse to a 'Command' to a waiter in 'm'.
type RouteF m = forall c. (Typeable (Out c)) => Command c -> Resp c -> m ()

-- | Send bytes to an 'Addr'ess in 'm'.
type SendF  m = Addr -> ByteString -> m ()


