{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

{-|
Stability : experimental

This module defines a Bucket ADT which can be used to store a collection of 'Contacts'.

- New contacts are 'enter'd into the Bucket and begin marked 'Good'.
- A single 'Bad' contact can be dropped with 'dropBad'. The Bucket may choose
  the 'lowest' priority Contact to drop.
- Buckets can be split into two by distance to an ID with 'split'.
- Questionable contacts can be checked by an external function which may mark
  them as Good or Bad in order to effect their standing in the Bucket.

Semi-careful use of these functions should allow maintaining collections of
Buckets of bounded size of Good Contacts. These properties can be used to build Routing
trees.

Note: Functionality here isn't quite complete. Observed usage will direct how this API ends up.
For now, 'modifyBucketContact' is the escape hatch to directly modify Contacts.
-}
module DHT.Bucket
  (-- * Bucket functions
    Bucket ()
  , emptyBucket

  -- ** Query state
  , contacts
  , lastUsed
  , bucketSize

  -- ** Update state
  , useBucket
  , updateBucket
  , split
  , enter
  , dropBad
  , modifyBucketContact
  ) where

import Data.List

import DHT.Address
import DHT.Contact
import DHT.ID
import DHT.Types

-- | A Bucket holds a fixed number of 'Contact's. It also tracks when it was last
-- used to allow refreshing when appropriate.
--
-- Operations which return Contacts from a Bucket should order by how long they've been good for
-- under the assumption that Contacts which have been around for a long time are likely to continue
-- to stick around and be good.
data Bucket = Bucket
  { _lastUsed :: Time
  , _contacts :: [Contact]
  }
  deriving Show

-- | The empty bucket at a given time period.
emptyBucket :: Time -> Bucket
emptyBucket t = Bucket t []

-- | The Buckets Contacts ordered by 'Goodness'.
contacts :: Bucket -> [Contact]
contacts = _contacts

-- | When the Bucket was last accessed or updated.
lastUsed :: Bucket -> Time
lastUsed = _lastUsed

-- | The number of Contacts currently stored.
bucketSize :: Bucket -> Int
bucketSize (Bucket _ cs) = length cs

-- | Update the lastUsed time if later.
useBucket :: Time -> Bucket -> Bucket
useBucket now (Bucket lu cs) = Bucket (if now > lu then now else lu) cs

-- | Split a Bucket by the distance bit at a given depth
split :: ID -> Int -> Bucket -> (Bucket,Bucket)
split ourID depth (Bucket t cs) =
  let (further,nearer) = partition (\contact -> (== Far) . leadingBit
                                                         . dropLeadingBits depth
                                                         . _unDistance
                                                         . distance (contactID contact)
                                                         $ ourID
                                   )
                                   cs

     in (Bucket t further, Bucket t nearer)

-- | Enter a new Contact into a Bucket. Mark the Bucket as fresh, the Contact is assumed to be Good.
enter :: ID -> Address -> Time -> Bucket -> Bucket
enter cID nAdr now (Bucket _ cs) = Bucket now . nub . (: cs) . replaceID cID . mkContact 8 $ nAdr
-- TODO:
-- - Overriding ID should be dropped or optional, not required!

-- | If the Bucket contains a bad Contact, drop it, prefering to drop a contact with lower priority.
dropBad :: Bucket -> Maybe Bucket
dropBad (Bucket lu neighbours) = Bucket <$> pure lu <*> f neighbours
  where f []     = Nothing
        f (k:ks)
          | isBad k   = Just ks
          | otherwise = do ks' <- f ks
                           Just $ k : ks'

-- | Update a bucket by running an update function one by one on any Questionable Contacts, setting
-- contacts which reply to Good. If a contact becomes Bad, it is returned and the update short-circuits.
updateBucket :: forall m. Monad m => Bucket -> (Address -> m Bool) -> m (Bucket,Bool)
updateBucket (Bucket lu ctcts) f = updateContacts ctcts >>= \(cs,contactDropped) -> return (Bucket lu cs,contactDropped)
  where
    updateContacts :: Monad m => [Contact] -> m ([Contact],Bool)
    updateContacts []     = return ([],False)
    updateContacts (c:cs) = do
      mc' <- updateContact c
      case mc' of
        Nothing -> return (cs,True)
        Just c' -> do
          (cs',contactDropped) <- updateContacts cs
          return (c':cs',contactDropped)

    -- Questionable Contacts which do not reply return Nothing.
    updateContact :: Monad m => Contact -> m (Maybe Contact)
    updateContact c
      | isQuestionable c = do b <- f (contactAddress c)
                              if b
                                then return . Just . setGood $ c
                                else return Nothing
      | otherwise = return $ Just c

-- | If a node with the given ID is stored by the bucket, modify it.
modifyBucketContact :: ID -> (Contact -> Contact) -> Bucket -> Maybe Bucket
modifyBucketContact _   _ (Bucket _ [])     = Nothing
modifyBucketContact tID f (Bucket lstUsd (c:cs))
  | contactID c == tID = Just $ Bucket lstUsd (f c : cs)
  | otherwise          = do Bucket l cs' <- modifyBucketContact tID f (Bucket lstUsd cs)
                            Just $ Bucket l (c:cs')

