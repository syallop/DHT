{-|
Stability : experimental

Defines a simple in-memory hashmap ValueStore for use in a DHT.
 -}
module DHT.SimpleNode.ValueStore
  (newSimpleValueStore
  )
  where

import DHT.Client
import DHT.Core.ID

import Control.Concurrent
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as Lazy

-- The state of stored values is a MVar reference to a Map
type ValState = MVar (Map.Map ID Lazy.ByteString)

-- Insert a new value
valInsert :: ValState -> ValInsertF IO
valInsert valState vID val = do
  vs <- takeMVar valState
  let vs' = Map.insert vID val vs
  putMVar valState vs'

-- Lookup a value
valLookup :: ValState -> ValLookupF IO
valLookup valState vID = do
  vs <- readMVar valState
  return $ Map.lookup vID vs

-- | Create a new empty ValueStore to be used by a DHT.
newSimpleValueStore :: IO (ValueStore IO)
newSimpleValueStore = do
  valState <- newMVar Map.empty
  pure $ mkValueStore (valInsert valState) (valLookup valState)

