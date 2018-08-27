{-|
Stability : experimental

Some common types used across a DHT.
 -}
module DHT.Types
  ( IP
  , Port
  , Time
  ) where

-- | An IP address should be in the format of an IPV4 string.
-- E.G. "127.0.0.1"
type IP = String

-- | A port number should be between 0 and 65535
type Port = Int

-- | Integer seconds since unix epoch
type Time = Integer

