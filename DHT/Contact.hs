{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
{-|
Stability : experimental

Record contact information for a DHT speaking node.
 -}
module DHT.Contact
  ( Goodness(..)
  , Contact(..)

  , showContact
  , showContacts
  ) where

import DHT.ID
import DHT.Address

import Data.Binary
import Data.Typeable
import Data.List
import GHC.Generics

-- | How good we consider a contact to be.
data Goodness
  -- | Good contacts respond to queries, make proper queries on us. Good contacts should not be replaced.
  = Good

  -- | Questionable contacts were once good but have since failed to respond correctly to our queries.
  -- Contacts which continue to fail will become 'Bad'.
  | Questionable

  -- | Bad contacts have failed to respond to multiple queries and may be replaced.
  | Bad
  deriving (Show,Read,Eq,Ord,Generic)
instance Binary Goodness

-- | A contact has an address and an ID we know them by along with a 'Goodness' ranking.
data Contact = Contact
  {_ID       :: ID
  ,_addr     :: Address
  ,_goodness :: Goodness
  }
  deriving (Show, Ord, Generic, Typeable)
instance Binary Contact

-- Contacts are compared for equality on their ID's ONLY
instance Eq Contact where
  (Contact id0 _ _) == (Contact id1 _ _) = id0 == id1

showContact :: Contact -> String
showContact (Contact i addr _) = concat ["<",show i,"@",show addr,">"]

showContacts :: [Contact] -> String
showContacts []     = "{}"
showContacts ns     = "{" ++ intercalate ", " (map showContact ns) ++ "}"

