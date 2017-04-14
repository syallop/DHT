{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

{-|
Stability : experimental

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
  let (further,nearer) = partition (\(Contact cID _ _) -> (== Far) . head . drop depth . distance cID $ ourID) cs
     in (Bucket t further,Bucket t nearer)

-- | Enter a new Contact into a Bucket. Mark the Bucket as fresh, the Contact is assumed to be Good.
enter :: ID -> Addr -> Time -> Bucket -> Bucket
enter cID nAdr now (Bucket _ cs) = Bucket now (nub (Contact cID nAdr Good : cs))

-- | If the Bucket contains a bad Contact, drop it, prefering to drop a contact with lower priority.
dropBad :: Bucket -> Maybe Bucket
dropBad (Bucket lu neighbours) = Bucket <$> pure lu <*> f neighbours
  where f []     = Nothing
        f (k@(Contact _ _ goodness):ks) = case goodness of
          Bad -> Just ks
          _   -> do ks' <- f ks
                    Just $ k : ks'

-- | Update a bucket by running an update function one by one on any Questionable Contacts, setting
-- contacts which reply to Good. If a contact becomes Bad, it is returned and the update short-circuits.
updateBucket :: forall m. Monad m => Bucket -> (Addr -> m Bool) -> m (Bucket,Bool)
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
    updateContact (Contact cID addr Questionable) = do
      b <- f addr
      if b then return $ Just $ Contact cID addr Good else return Nothing
    updateContact c = return $ Just c

-- | If a node with the given ID is stored by the bucket, modify it.
modifyBucketContact :: ID -> (Contact -> Contact) -> Bucket -> Maybe Bucket
modifyBucketContact _   _ (Bucket _ [])     = Nothing
modifyBucketContact tID f (Bucket lstUsd (c:cs))
  | _ID c == tID = Just $ Bucket lstUsd (f c : cs)
  | otherwise    = do Bucket l cs' <- modifyBucketContact tID f (Bucket lstUsd cs)
                      Just $ Bucket l (c:cs')

