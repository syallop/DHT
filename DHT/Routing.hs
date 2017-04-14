{-# LANGUAGE
    PatternSynonyms
  , ScopedTypeVariables
  #-}
{-|
Stability : experimental

A 'Routing' table is owned by an ID and remembers 'Contacts', many which are near by the distance metric
and few which are futher away. Contacts will not be kept if:
  - They are indicated to have become 'Bad'
  - They are far away and we already know of a Buckets worth of Contacts in that area
  - Otherwise, progressivly nearer Contacts are always kept and may cause a Bucket split
    (which could make room for up to a Buckets worth of contacts that we didnt want before.)
-}
module DHT.Routing
  (-- * Core functions on Routing tables
    Routing ()
  , empty
  , insert
  , inserts
  , lookup
  , modify

  -- * Queries on configuration
  , maxBucketSize
  , ourRoutingID

  -- * Extras!
  , toList
  , depth
  , depthNear
  , bucketCount
  ) where

import Prelude hiding (lookup)

import Data.Foldable              (foldrM)
import Data.List           hiding (insert,lookup)

import DHT.Bucket
import DHT.Contact
import DHT.ID
import DHT.Types

-- A tree of Buckets which store up to a fixed number of Contacts.
-- Buckets are stored at the leaves, branches have a near and a far branch.
-- 'further' corresponds to a leading 0 bit in the distance metric of all the IDs stored below.
-- 'nearer' corresponds to a leading 1 bit in the distance metric of all the IDS stored below.
data Tree
  = Leaf
    { _bucket :: Bucket
    }

  | Branch
    { _further :: !Tree
    , _nearer  :: !Tree
    }
    deriving Show

-- | Maintain a collection of encountered 'Contact's, grouped into 'Bucket's
-- by their distance to the owner such that a lot of close Contacts are kept around but excess
-- contacts which are further away are dropped.
--
-- I.E. This structure will not keep all Contacts 'insert'ed, biased towards ones which are near and 'Good'.
data Routing = Routing
  { _maxBucketSize :: Int  -- ^ Maximum quantity of Contacts stored in Buckets
  , _ourID         :: ID   -- ^ Our own ID
  , _tree          :: Tree -- ^ The routing tree we maintain
  }

-- | The deepest leaf
depth :: Routing -> Int
depth (Routing _ _ t) = depth' t
  where depth' (Leaf _)     = 1
        depth' (Branch f n) = 1 + max (depth' f) (depth' n)

-- | Depth of the near path
depthNear :: Routing -> Int
depthNear (Routing _ _ t) = depthNear' t
  where depthNear' (Leaf _)     = 1
        depthNear' (Branch _ n) = 1 + depthNear' n

-- | The number of 'Bucket's (which themselves may contain 0 - maxbucketsize worth of 'Contact's)
bucketCount :: Routing -> Int
bucketCount (Routing _ _ t) = bucketCount' t
  where bucketCount' (Leaf _)     = 1
        bucketCount' (Branch f n) = bucketCount' f + bucketCount' n

-- | All stored 'Contact's ordered by their position in whatever 'Bucket' they're in, concatenated from
-- their leaves in the routing tree left-to-right/ furthest-to-nearest.
toList :: Routing -> [Contact]
toList (Routing _ _ t) = toList' t
  where toList' (Leaf b)     = contacts b
        toList' (Branch f n) = toList' f ++ toList' n

-- | The maximum number of 'Contacts's to keep in a single 'Bucket'.
-- Also the amount of Contactss that will be returned in lookups if available.
maxBucketSize :: Routing -> Int
maxBucketSize = _maxBucketSize

-- | The 'ID' of the owner of the Routing table by which all stored 'Contact's are relative.
ourRoutingID :: Routing -> ID
ourRoutingID = _ourID

-- | An initial, empty routing structure (Does NOT contain ourself).
empty :: Int -> ID -> Time -> Routing
empty maxSize ourID now = Routing maxSize ourID $ Leaf $ emptyBucket now


{- INSERT -}
-- A path down a binary tree is a bitstring
type Path = Bits
pattern L = False
pattern R = True

-- Move left down a path
moveL :: Path -> Path
moveL = ( ++ [L])

-- Move right down a path
moveR :: Path -> Path
moveR = ( ++ [R])

-- Is a path moving towards the most near path (1:1:1:...)
towardsNearest :: Path -> Bool
towardsNearest = all (== R)

-- Is a target ID in the range of an ID, starting at the given Path?
inRange :: ID -> ID -> Path -> Bool
inRange target us path = towardsNearest path && ((== Near) . head . drop (length path) . distance target $ us)

-- | Insert an 'Addr'ess at a given 'Time'.
--
-- - If the Addr is along a near path the 'Contact' will be inserted even if it requires the 'Bucket' be split.
-- - If the Addr is far away and we already have a full 'Bucket' of similarly far neighbours:
--   - If there is a Bad Contact then it will replace it
--   - If there are any Questionable Contacts then they will be updated with the given ping function one by one. If one becomes Bad it will be replaced,
--     otherwise the inserted Contact is not inserted.
insert :: forall m. Monad m => Addr -> Time -> (Addr -> m Bool) -> Routing -> m Routing
insert cAddr now ping rt = finalRt
  where
    cID           = mkID cAddr (maxBucketSize rt)
    totalDistance = distance cID $ ourRoutingID rt
    finalTree     = insertTree totalDistance [] (_tree rt)
    finalRt       = finalTree >>= \t -> return rt{_tree = t}

    insertTree :: Distance -> Path -> Tree -> m Tree
    insertTree dst pth (Leaf bu)
      | bucketSize bu < _maxBucketSize rt
       = return $ Leaf $ enter cID cAddr now bu

      -- look for bad contacts to make room
      | otherwise
       = case dropBad bu of

           -- A bad contact was dropped, making room
           Just bu' -> return $ Leaf $ enter cID cAddr now bu'

           -- No bad contacts. Update questionable contacts one by one to see if room can be made
           Nothing  -> do
            (bu',contactDropped) <- updateBucket bu ping
            if contactDropped
              then return $ Leaf $ enter cID cAddr now bu'

               -- No contacts became bad. We can split the bucket if we're in range but otherwise
               -- the ID is dropped.
               else if inRange cID (ourRoutingID rt) pth
                      then let (further,nearer) = split (ourRoutingID rt) (length pth) bu'
                              in insertTree dst pth $ Branch (Leaf further) (Leaf nearer)
                      else return $ Leaf bu'

    insertTree (Near : ds) pth (Branch further nearer) = do
      nearer' <- insertTree ds (moveR pth) nearer
      return $ Branch further nearer'

    insertTree (Far  : ds) pth (Branch further nearer) = do
      further' <- insertTree ds (moveL pth) further
      return $ Branch further' nearer

    -- impossible
    insertTree [] _ _ = error "insertTree: calculated distance too short"
    insertTree _ _ _  = error "insertTree"

-- | 'insert' multiple 'Addr's at the same 'Time'.
inserts :: Monad m => [Addr] -> Time -> (Addr -> m Bool) -> Routing -> m Routing
inserts cAddrs now ping rt = foldrM (\cAddr accRt -> insert cAddr now ping accRt) rt cAddrs

-- sort a list of Contact's by their distance to an ID
sortTo :: ID -> [Contact] -> [Contact]
sortTo targetID = map fst . sortBy (\(_,d) (_,d') -> compare d' d) . map (\c@(Contact cID _ _) -> (c,distance targetID cID))

-- sort a list of Contact's by their distance to an ID. Separate an exact match.
sortFor :: ID -> [Contact] -> ([Contact],Maybe Contact)
sortFor targetID ctcts = case sortTo targetID ctcts of
  []                    -> ([],Nothing)
  (c:cs)
    | _ID c == targetID -> (cs,Just c)
    | otherwise         -> (c:cs,Nothing)

{-LOOKUP-}

-- intermediate structure of a Contact lookup, either the target Contact is:
data Lookup
  = Found Contact [Contact] Int -- found and we've collected a number of neighbouring Contacts with a number to try and still find
  | NotFound      [Contact] Int -- or it is not found yet and we may start collecting neighbours if it does not exist anywhere
                                -- , with a number to try and still find

-- | Lookup the 'Contact' of the target 'ID' and the nearest neighbour Contacts
-- Accessed 'Bucket's are updated.
lookup :: Addr -> ID -> Time -> Routing -> (Routing,([Contact], Maybe Contact))
lookup enquirerAddr targetID now rt =
  let (finalTree,lk) = lookupTree totalDistance (_tree rt) (NotFound [] size)
      rt' = rt{_tree = finalTree}
     in case lk of
               NotFound cs _ -> (rt',(sortTo targetID cs,Nothing))
               Found c  cs _  -> (rt',(sortTo targetID cs,Just c))
  where
    totalDistance = distance (mkID enquirerAddr $ maxBucketSize rt) targetID
    size          = maxBucketSize rt

    lookupTree :: Distance -> Tree -> Lookup -> (Tree,Lookup)

    -- Reached a Leaf
    lookupTree _ (Leaf bu) lk = case lk of

      -- We've found the contact in another leaf
      Found c cs i
        -- but havnt collected enough neighbours yet
        | 0 < i     -> let cs' = take i (contacts bu)
                           i'  = i - length cs'
                          in (Leaf $ useBucket now bu, Found c (cs ++ cs') i')

        -- and are done
        | otherwise -> (Leaf bu,Found c cs i)

      -- We've not found the contact => it will be in this leaf or nowhere
      NotFound cs i
        -> let (cs',mc) = sortFor targetID (contacts bu)
              in case mc of
                     -- Not in tree, collect closest neighbours
                     Nothing -> let i' = i - length cs'
                                   in (Leaf $ useBucket now bu, NotFound (cs ++ cs') i')

                     -- Found! Also collect closest neighbours
                     Just c  -> let i' = i - (1 + length cs')
                                   in (Leaf $ useBucket now bu, Found c (cs ++ cs') i')


    -- Looking for contact/ more neighbours in the Near branch if possible
    -- TODO: Maybe merge with Far branch which does the exact opposite
    lookupTree (Near:ds) (Branch ls rs) lk =
        let (rs',lk') = lookupTree ds rs lk
           in case lk' of -- TODO: merge cases on i

                -- We found it down its branch
                Found _ _ i
                    -- but didnt collect enough neighbours anywhere below it
                    -- => we can only collect from the opposite branch
                    | 0 < i     -> let (ls',lk'') = lookupTree ds ls lk'
                                      in (Branch ls' rs',lk'')

                    -- and collected enough somewhere near
                    | otherwise -> (Branch ls rs',lk')

                -- Its not in the tree
                NotFound _ i
                    -- and we didnt collect enough neighbours around where it would have been
                    -- => we can only collect from the opposite branch
                    | 0 < i     -> let (ls',lk'') = lookupTree ds ls lk'
                                      in (Branch ls' rs',lk'')

                    -- but collected enough somewhere near
                    | otherwise -> (Branch ls rs', lk')

    -- Looking for contact/ more neighbours in the Far branch if possible
    lookupTree (Far:ds) (Branch ls rs) lk =
        let (ls',lk') = lookupTree ds ls lk
           in case lk' of -- TODO: merge cases on i

                -- We found it down its branch
                Found _ _ i
                    -- but didnt collect enough neighbours anywhere below it
                    -- => we can only collect from the opposite branch
                    | 0 < i     -> let (rs',lk'') = lookupTree ds rs lk'
                                      in (Branch ls' rs',lk'')

                    -- and collected enough somewhere near
                    | otherwise -> (Branch ls' rs,lk')

                -- Its not in the tree
                NotFound _ i
                    -- and we didnt collect enough neighbours around where it would have been
                    -- => we can only collect from the opposite branch
                    | 0 < i     -> let (rs',lk'') = lookupTree ds rs lk'
                                      in (Branch ls' rs',lk'')

                    -- but collected enough somewhere near
                    | otherwise -> (Branch ls' rs, lk')

    lookupTree [] _ _ = error "lookupTree: calculated distance too short"
    lookupTree _  _ _ = error "lookupTree"

-- | If there is a 'Contact' with the given 'ID', then modify it.
modify :: ID -> (Contact -> Contact) -> Routing -> Maybe Routing
modify targetID cF rt = do
  tree' <- modifyTree totalDistance (_tree rt)
  Just $ rt{_tree = tree'}
  where
    totalDistance = distance targetID $ ourRoutingID rt

    modifyTree :: Distance -> Tree -> Maybe Tree
    modifyTree dst t = case t of
      Leaf bu
        -> Leaf <$> modifyBucketContact targetID cF bu

      Branch further nearer
        -> case dst of
             (Near : ds) -> do nearer' <- modifyTree ds nearer
                               Just $ Branch further nearer'

             (Far  : ds) -> do further' <- modifyTree ds further
                               Just $ Branch further' nearer

             [] -> error "modifyTree: calculated distance too short"
             _  -> error "modifyTree"

