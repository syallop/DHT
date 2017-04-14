{-# LANGUAGE
    PatternSynonyms
   ,TypeOperators
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
  ,pattern Zero
  ,pattern One
  ,Bits
  ,showBit
  ,showBits
  ,dist
  ,distance

  ,ID
  ,mkID

  ,Distance
  ,pattern Far
  ,pattern Near

  ,toBits
  ,fromBits
  ) where

import Data.List
import Data.Hashable
import qualified Data.Bits as B

-- | A binary digit
type Bit = Bool

pattern Zero :: Bool
pattern Zero = False

pattern One :: Bool
pattern One  = True

-- | A string of 'Bit's
type Bits = [Bit]

-- convert an Int to Bits truncated to the given size
toBits :: Int -> Int -> Bits
toBits i size = map (B.testBit i) [0..size]

-- convert Bits to an Int (assuming it doesnt overflow)
fromBits :: Bits -> Int
fromBits bs = foldl' (\acc (i,b) -> if b then B.setBit acc i else acc) 0 $ zip [0..(length bs)] bs

showBit :: Bit -> String
showBit b = if b then "1" else "0"

showBits :: Bits -> String
showBits = concatMap showBit

-- | The distance between two single Bit's is an XOR
dist :: Bit -> Bit -> Bit
dist x y
  | x == y    = Zero
  | otherwise = One

-- | The distance between two bitstrings is a bitstring of XORs
distance :: Bits -> Bits -> Bits
distance [] [] = []
distance (x:xs) (y:ys) = x `dist` y : distance xs ys
distance _ _ = error "distance: IDs have different length"


-- | An ID is a string of Bits used to uniquely identify a resource
type ID = Bits

-- | A Hashable type can be converted to an ID of a given size.
mkID :: Hashable a => a -> Int -> ID
mkID h = toBits (hash h)


-- | The distance between two bitstrings is a bitstring.
type Distance = Bits
pattern Far :: Bool
pattern Far  = False

pattern Near :: Bool
pattern Near = True

