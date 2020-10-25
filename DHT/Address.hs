{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
{-|
Stability : experimental

An Address should fully describe a port of communication.

E.G. An Address of "127.0.0.1" is insufficient if the communicator must assume
port 80, and protocol UDP.
-}
module DHT.Address
  ( Address (..)
  , AddressPart (..)
  , parts
  , fromParts
  ) where

import DHT.Types
import Data.Binary
import Data.Hashable
import GHC.Generics

-- | A single component of an address.
data AddressPart
  = IPV4 IP
  | IPV6 IP
  | UDP Port
  | TCP Port
  deriving (Eq,Ord,Generic)
instance Binary AddressPart

instance Show AddressPart where
  show addrPart = case addrPart of
    IPV4 ip
      -> ip

    IPV6 ip
      -> ip

    UDP port
      -> show $ port

    TCP port
      -> show $ port

instance Hashable AddressPart where
  hashWithSalt s addr = case addr of
    IPV4 ip
      -> s `hashWithSalt` ip

    IPV6 ip
      -> s `hashWithSalt` ip

    UDP port
      -> s `hashWithSalt` port

    TCP port
      -> s `hashWithSalt` port

-- | An Address should fully describe how to communicate with some entity.
--
-- E.G. An Address of "127.0.0.1" is insufficient if the communicator must
-- assume port 80 and protocol UDP.
data Address
  = Address AddressPart
  | AddressThen AddressPart Address
  deriving (Eq, Ord, Generic)

instance Binary Address

instance Hashable Address where
  hashWithSalt s addr = case addr of
    Address addrPart
      -> s `hashWithSalt` addrPart

    AddressThen addrPart thenAddr
      -> s `hashWithSalt` addrPart `hashWithSalt` thenAddr

instance Show Address where
  show addr = case addr of
    Address part
      -> show part

    AddressThen part thenAddr
      -> show part <> ">" <> show thenAddr

-- | Break an Address into a (non-empty) sequence of it's AddressParts.
parts :: Address -> [AddressPart]
parts address = case address of
  Address part
    -> [part]

  AddressThen part thenAddress
    -> part : parts thenAddress

-- | Construct an Address from a (non-empty) sequence of it's AddressParts.
fromParts :: AddressPart -> [AddressPart] -> Address
fromParts part1 parts = case parts of
  []
    -> Address part1

  (p:ps)
    -> AddressThen part1 (fromParts p ps)

