{-# LANGUAGE
    PatternSynonyms
  , TypeOperators
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}
{-|
Stability : experimental

An ID is a fixed length string of 'Bit's that identifies a resource (a node, a stored value, etc) in a DHT.

There is a notion of 'distance' between ID's such that the distance from A to B will always be less than (or equal)
to the total distance A to C to B.
 -}
module DHT.ID
  (-- * Bits
    Bit
  , pattern Zero
  , pattern One
  , Bits (Bits)
  , xor
  , distance

  , ID
  , mkID

  , Distance (Distance)
  , _unDistance
  , pattern Far
  , pattern Near

  , toBits
  , fromBits

  , lengthBits
  , dropLeadingBits
  , leadingBit
  ) where

import Data.Binary
import Data.Hashable

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import DHT.Bits

-- | An ID is a string of Bits used to uniquely identify a resource.
newtype ID = ID {_unID :: Bits}
  deriving (Eq, Ord, Binary)

instance Show ID where
  show (ID bs) = show bs

-- | A Hashable type can be converted to an ID of a given size.
mkID :: Hashable a => a -> Int -> ID
mkID h = ID . toBits (hash h)

-- | The distance between two bitstrings is a bitstring.
newtype Distance = Distance {_unDistance :: Bits}
  deriving (Eq,Ord,Generic,NFData)

pattern Far :: Bit
pattern Far = Zero

pattern Near :: Bit
pattern Near = One

-- | The distance between two bitstrings is a bitstring of XORs.
distance :: ID -> ID -> Distance
distance (ID (Bits bs0)) (ID (Bits bs1)) = Distance $ Bits $ distance' bs0 bs1
  where
    distance' :: [Bit] -> [Bit] -> [Bit]
    distance' []     []     = []
    distance' (x:xs) (y:ys) = x `xor` y : distance' xs ys
    distance' _ _ = error "distance: IDs have different length"

