{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  #-}
{-|
Stability : experimental

Record contact information for a DHT speaking node.
 -}
module DHT.Core.Contact
  ( Goodness
      (Good
      ,Questionable
      ,Bad)

  , Contact ()
  , mkContact
  , replaceID

  , contactID
  , contactAddress
  , contactGoodness

  , updateGoodness
  , setGood
  , setBad
  , setQuestionable
  , isGood
  , isBad
  , isQuestionable

  , idsEqual
  , exactlyEqual

  , showContact
  , showContacts
  ) where

import DHT.Core.ID
import DHT.Core.Address

import Data.Binary
import Data.Typeable
import Data.List
import GHC.Generics

-- TODO:
-- - Should we handle Contacts with multiple addresses?
-- - Should we enforce Contacts ID are created from addresses?

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

-- | Create a Contact whose ID is automatically derived from their Address (with
-- a given size).
--
-- If a disconnected ID is required, use 'replaceID'.
mkContact
  :: Int
  -> Address
  -> Contact
mkContact size addr = Contact (mkID addr size) addr Good

-- | If a Contact's ID should _not_ be derived from it's address, it can be
-- replaced.
--
-- If this is used, the caller takes responsibility for ensuring similar
-- properties to a hash function on the address, I.E.:
-- - ID's are normally distributed across Contacts (collisions are tolerated but
--   should often be minimised as much as is reasonable).
-- - Contacts ID's remain stable
-- - IDs are recomputed to the same value for the same Contact.
replaceID
  :: ID
  -> Contact
  -> Contact
replaceID cId (Contact _oldID addr goodness) = Contact cId addr goodness

-- | Access a Contacts ID.
contactID
  :: Contact
  -> ID
contactID = _ID

-- | Access a Contacts Address.
contactAddress
  :: Contact
  -> Address
contactAddress = _addr

-- | Access a Contacts Goodness.
contactGoodness
  :: Contact
  -> Goodness
contactGoodness = _goodness

-- | Update the Goodness value of a Contact.
updateGoodness
  :: (Goodness -> Goodness)
  -> Contact
  -> Contact
updateGoodness f (Contact cId addr goodness) = Contact cId addr (f goodness)

setGood :: Contact -> Contact
setGood = updateGoodness (const Good)

setBad :: Contact -> Contact
setBad = updateGoodness (const Bad)

setQuestionable :: Contact -> Contact
setQuestionable = updateGoodness (const Questionable)

isGood :: Contact -> Bool
isGood = (== Good) . contactGoodness

isBad :: Contact -> Bool
isBad = (== Bad) . contactGoodness

isQuestionable :: Contact -> Bool
isQuestionable = (== Questionable) . contactGoodness

-- | Do two contact have the same ID?
-- Does not consider any other properties for equality, unlike ==.
idsEqual :: Contact -> Contact -> Bool
idsEqual (Contact id0 _ _) (Contact id1 _ _) = id0 == id1

-- | Are two contacts equal on all properties, I.E. not just their ID?
exactlyEqual :: Contact -> Contact -> Bool
exactlyEqual (Contact id0 addr0 goodness0) (Contact id1 addr1 goodness1) = and
  [id0       == id1
  ,addr0     == addr1
  ,goodness0 == goodness1
  ]

instance Eq Contact where (==) = idsEqual

showContact :: Contact -> String
showContact (Contact i addr _) = concat ["<",show i,"@",show addr,">"]

showContacts :: [Contact] -> String
showContacts []     = "{}"
showContacts ns     = "{" ++ intercalate ", " (map showContact ns) ++ "}"

