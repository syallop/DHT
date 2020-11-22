{-|
Stability : experimental

Exports ValueStore which encapsulates the operations we must know how to perform,
in order to store and lookup values.
 -}
module DHT.Client.Op.ValueStore
  ( ValueStore()
  , mkValueStore

  , ValInsertF
  , ValLookupF

  , insert
  , lookup
  ) where

import DHT.Core.ID

import Data.ByteString.Lazy.Char8 (ByteString)
import Prelude hiding (insert, lookup)

-- TODO:
-- - Insert and lookup are bad names?

-- | Operations on value storage
data ValueStore m = ValueStore
    { _valueStoreInsert :: ValInsertF m -- ^ Insert a value in the storage
    , _valueStoreLookup :: ValLookupF m -- ^ Lookup a value in the storage
    }

-- | Construct a ValueStore by providing operations for inserting and looking up
-- ID<->ByteString mappings.
mkValueStore
  :: ValInsertF m -- ^ Insert a value in the storage
  -> ValLookupF m -- ^ Lookup a value in the storage
  -> ValueStore m
mkValueStore = ValueStore

-- | Insert with a ByteString value with the given ID in 'm'.
type ValInsertF m = ID -> ByteString -> m ()

-- | Lookup a ByteString value with a given ID in 'm'.
type ValLookupF m = ID -> m (Maybe ByteString)

-- | Insert a value into the storage.
insert
  :: ValueStore m
  -> ValInsertF m
insert = _valueStoreInsert

-- | Lookup a value in the storage.
lookup
  :: ValueStore m
  -> ValLookupF m
lookup = _valueStoreLookup

