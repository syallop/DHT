{-|
Stability : experimental

Record contact information for a DHT speaking node.
 -}
module DHT.Contact
  (Addr(..)
  ,Goodness(..)
  ,Contact(..)

  ,showContact
  ,showContacts
  ) where

import DHT.ID
import DHT.Types

import Data.Hashable
import Data.List

-- | The address at which a contact can be found
data Addr = Addr IP Port
  deriving (Show,Read,Eq,Ord)

instance Hashable Addr where
  hashWithSalt s (Addr ip port) = s `hashWithSalt` ip `hashWithSalt` port

showAddr :: Addr -> String
showAddr (Addr ip port) = ip ++ "::" ++ show port

-- | How good we consider a contact to be.
data Goodness
  -- | Good contacts respond to queries, make proper queries on us. Good contacts should not be replaced.
  = Good

  -- | Questionable contacts were once good but have since failed to respond correctly to our queries.
  -- Contacts which continue to fail will become 'Bad'.
  | Questionable

  -- | Bad contacts have failed to respond to multiple queries and may be replaced.
  | Bad
  deriving (Show,Read,Eq,Ord)

-- | A contact has an address and an ID we know them by along with a 'Goodness' ranking.
data Contact = Contact
  {_ID       :: ID
  ,_addr     :: Addr
  ,_goodness :: Goodness
  }
  deriving (Show,Read,Ord)

-- Contacts are compared for equality on their ID's ONLY
instance Eq Contact where
  (Contact id0 _ _) == (Contact id1 _ _) = id0 == id1

showContact :: Contact -> String
showContact (Contact id addr _) = concat ["<",showBits id,"@",showAddr addr,">"]

showContacts :: [Contact] -> String
showContacts []     = "{}"
showContacts ns     = "{" ++ intercalate ", " (map showContact ns) ++ "}"

