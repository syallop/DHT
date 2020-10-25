{-# LANGUAGE
    PatternSynonyms
  , TypeOperators
  , GeneralizedNewtypeDeriving
  #-}
{-|
Stability : experimental

Bits are are a string of 1's and 0's.

Construct from Ints with 'toBits' and a padding/ truncation length.

Bits with extra padding are NOT equal, I.E.
00101 /= 101

The 'leading' bit is the leftmost bit.

 -}
module DHT.Bits
  (-- * Bits
    Bit
  , pattern Zero
  , pattern One
  , Bits (Bits)
  , _unBits
  , toBits
  , fromBits
  , xor
  , lengthBits
  , dropLeadingBits
  , leadingBit
  ) where

import Data.Binary
import Data.List
import qualified Data.Bits as B

-- | A binary digit.
newtype Bit = Bit {_unBit :: Bool}
  deriving (Eq, Ord, Binary)

instance Show Bit where
  show (Bit False) = "0"
  show (Bit True)  = "1"

-- | Zero = False
pattern Zero :: Bit
pattern Zero = Bit False

-- | One = True
pattern One :: Bit
pattern One = Bit True

-- | A string of 'Bit's.
newtype Bits = Bits {_unBits :: [Bit]}
  deriving (Eq, Ord, Binary)

instance Show Bits where
  show (Bits bs) = concatMap show bs

-- | The number of individual Bits.
lengthBits :: Bits -> Int
lengthBits (Bits bs) = length bs

-- | Drop a number of leading prefix bits.
dropLeadingBits :: Int -> Bits -> Bits
dropLeadingBits i (Bits bs) = Bits . drop i $ bs

-- | Index the leading bit.
leadingBit :: Bits -> Bit
leadingBit (Bits bs) = head bs

-- | Convert an Int to Bits truncated to the given size.
toBits :: Int -> Int -> Bits
toBits i size
  | size <= 0 = Bits []
  | otherwise = Bits . reverse . map (Bit . B.testBit i) $ [0..size-1]

-- | Convert Bits to an Int (assuming it doesnt overflow).
fromBits :: Bits -> Int
fromBits (Bits bs) = foldl' (\acc (i,Bit b) -> if b then B.setBit acc i else acc)
                            0
                            . zip [0..(length bs)]
                            . reverse
                            $ bs

-- | Exclusive-or two individual bits.
xor :: Bit -> Bit -> Bit
xor x y
  | x == y    = Zero
  | otherwise = One

