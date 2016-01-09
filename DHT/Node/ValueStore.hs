{-|
Stability : experimental

Defines a simple in-memory hashmap ValueStore for use in a DHT.
 -}
module DHT.Node.ValueStore
  (newValueStore
  )
  where

import DHT
import DHT.ID

import Control.Applicative
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
newValueStore :: IO (ValueStore IO)
newValueStore = mkValueStore <$> newValState
  where
    mkValueStore valState = ValueStore (valInsert valState) (valLookup valState)

    newValState :: IO ValState
    newValState = newMVar Map.empty

