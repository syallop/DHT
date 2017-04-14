{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  #-}
{-|
Stability : experimental

Exports ValueStoreOp which encapsulates the operations we must know how to perform,
in order to store and lookup values.
 -}
module DHT.Op.ValueStore
  (ValueStoreOp(..)
  ,ValInsertF
  ,ValLookupF
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

-- | Operations on value storage
data ValueStoreOp m = ValueStoreOp
    { _valueStoreOpInsert :: ValInsertF m -- ^ Insert a value in the storage
    , _valueStoreOpLookup :: ValLookupF m -- ^ Lookup a value in the storage
    }

-- | Insert with a ByteString value with the given ID in 'm'.
type ValInsertF m = ID -> ByteString -> m ()

-- | Lookup a ByteString value with a given ID in 'm'.
type ValLookupF m = ID -> m (Maybe ByteString)

