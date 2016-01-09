{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  #-}
{-|
Stability : experimental

Model a DHT and its common operations leaving configuration 'holes' for
dependencies ('Messaging','RoutingTable','ValueStore','Logging') which are fulfilled
when the final DHT program is executed with 'runDHT'.
 -}
module DHT
  (-- * DHT and configuration types
   DHT()
  ,DHTError(..)
  ,DHTConfig(..)
  ,Messaging(..)
  ,RoutingTable(..)
  ,ValueStore(..)
  ,Logging(..)
  ,SendF,WaitF,RouteF,RTInsertF,RTLookupF,ValInsertF,ValLookupF

  -- ** DHT functions
  ,runDHT
  ,handleMessage
  ,quitDHT

  ,bootstrap
  ,liftDHT

  -- *** Core Operations
  ,ping
  ,store
  ,findValue
  ,findContact

  -- *** Query the local state
  ,askOurID
  ,askOurAddr
  ,kSize
  ,lookupValue
  ,lookupContact

  -- *** Additional 'extra' operations, exposed for convenience
  ,timeNow
  ,randomInt
  ,lg
  )
  where

import Control.Applicative
import Control.Arrow              (first)
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List                  ((\\),nub)
import Data.Typeable

import DHT.Bucket
import DHT.Command
import DHT.Contact
import DHT.ID
import DHT.Message
import DHT.Types

{- DHT configuration components -}

-- | Messaging operations.
-- In some 'm' provide functions for:
data Messaging m = Messaging
  {_waitResponse  :: WaitF m  -- ^ Waiting for a response to a send message
  ,_routeResponse :: RouteF m -- ^ Routing a recieved response to a waiter

  ,_sendBytes     :: SendF m  -- ^ Physically sendind bytes to an address
  }

-- | Wait on the 'Out'put response in 'm' to a sent 'Command' with 'In'put.
type WaitF  m = forall c. Typeable (Out c) => Command c -> In c -> m (Out c)

-- | Route a 'Resp'onse to a 'Command' to a waiter in 'm'.
type RouteF m = forall c. (Typeable (Out c)) => Command c -> Resp c -> m ()

-- | Send bytes to an 'Addr'ess in 'm'.
type SendF  m = Addr -> ByteString -> m ()

-- | A Possible logging function in 'm'.
type Logging m = Maybe (String -> m ())

-- | Operations on a routing table
data RoutingTable m = RoutingTable
    { _rtInsert :: RTInsertF m -- ^ Insert a new 'Addr'ess
    , _rtLookup :: RTLookupF m -- ^ Lookup an 'Addr'ess
    , _rtKSize  :: m Int       -- ^ Query the bucket size/ how many neighbours we expect to get back
    }

-- | Insert an 'Addr'ess at a 'Time'. Ping Questionable Contacts with the given function if needed.
type RTInsertF m = Addr -> Time -> (Addr -> DHT m Bool) -> DHT m ()

-- | Lookup an 'Addr'ess and neighbouring Contacts to an 'ID' in 'm'.
type RTLookupF m = Addr -> ID -> Time -> m ([Contact],Maybe Contact)

-- | Operations on value storage
data ValueStore m = ValueStore
    { _valInsert :: ValInsertF m -- ^ Insert a value in the storage
    , _valLookup :: ValLookupF m -- ^ Lookup a value in the storage
    }

-- | Insert with a ByteString value with the given ID in 'm'.
type ValInsertF m = ID -> ByteString -> m ()

-- | Lookup a ByteString value with a given ID in 'm'.
type ValLookupF m = ID -> m (Maybe ByteString)

-- | DHT configuration
data DHTConfig m = DHTConfig
    {_time         :: m Time         -- ^ The current time
    ,_randomInt    :: m Int          -- ^ A random int

    ,_messaging    :: Messaging m    -- ^ A messaging system for sending, waiting and rouing messages.
    ,_routingTable :: RoutingTable m -- ^ A routing table system for tracking 'Contact's.
    ,_valueStore   :: ValueStore m   -- ^ A value storage and retrieval system.
    ,_logging      :: Logging m      -- ^ A possible logging function

    ,_askOurID     :: ID             -- ^ Our ID
    ,_askOurAddr   :: Addr           -- ^ Our Addr
    }

{- The DHT type, instances and primitive operations -}

-- | DHT errors
data DHTError
  = ETimeOut         -- ^ Timed out waiting for a response
  | EInvalidResponse -- ^ Invalid response

-- | A computation in the context of some DHTConfig executes in 'm' and either
-- shortcircuits to a DHTError (with the monad instance) or returns an 'a'.
newtype DHT m a = DHT {_runDHT :: DHTConfig m -> m (Either DHTError a)}

instance Monad m => Monad (DHT m) where
  return a = DHT $ \_ -> return $ Right a

  (DHT ma) >>= f = DHT $
    \r -> do res <- ma r
             case res of
               Left err -> return $ Left err
               Right a  -> _runDHT (f a) r

instance (Monad m,Functor m) => Applicative (DHT m) where
  pure  = return
  (<*>) = ap

instance Functor m => Functor (DHT m) where
  fmap f (DHT d) = DHT $ \r -> fmap f <$> d r

-- convenience for success as in inside a DHT after reading a DHTConfig.
succeed :: Monad m => a -> m (Either DHTError a)
succeed = return . Right

-- convenience for failure as in inside a DHT after reading a DHTConfig
failure :: Monad m => DHTError -> m (Either DHTError a)
failure = return . Left

-- | Lift an action in 'm' into DHT.
liftDHT :: Functor m => m a -> DHT m a
liftDHT = DHT . const . (Right <$>)

-- | Join an 'm a' inside a DHT to the outer context.
joinDHT :: (Monad m,Functor m) => DHT m (m a) -> DHT m a
joinDHT = (>>= liftDHT)

-- | Ask for the DHTConfig
ask :: Monad m => DHT m (DHTConfig m)
ask = DHT succeed

-- | Our own ID
askOurID :: Monad m => DHT m ID
askOurID = liftM _askOurID ask

-- | Our own address
askOurAddr :: Monad m => DHT m Addr
askOurAddr = liftM _askOurAddr ask

-- ask for the messaging system
askMessaging :: (Monad m,Functor m) => DHT m (Messaging m)
askMessaging = _messaging <$> ask

-- ask for the routing table system
askRoutingTable :: (Monad m,Functor m) => DHT m (RoutingTable m)
askRoutingTable = _routingTable <$> ask

-- ask for the value storage systen
askValueStore :: (Monad m,Functor m) => DHT m (ValueStore m)
askValueStore = _valueStore <$> ask

-- ask for the logging system
askLogging :: (Monad m,Functor m) => DHT m (Logging m)
askLogging = _logging <$> ask


-- | End a DHT with a DHTError.
dhtError :: Monad m => DHTError -> DHT m a
dhtError = DHT . const . failure

-- | End a DHT with a ETimeOut.
timeOut :: Monad m => DHT m a
timeOut = dhtError ETimeOut

-- | End a DHT with an EInvalidResponse.
invalidResponse :: Monad m => DHT m a
invalidResponse = dhtError EInvalidResponse


-- | The current time
timeNow :: (Monad m,Functor m) => DHT m Time
timeNow = ask >>= liftDHT . _time

-- | A random Int
randomInt :: (Monad m,Functor m) => DHT m Int
randomInt = ask >>= liftDHT . _randomInt


-- wait for a response to a command
waitResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> In c -> DHT m (Out c)
waitResponse cmd i = askMessaging >>= \msgsys -> liftDHT $ _waitResponse msgsys cmd i

-- route a response to the correct waiing location
routeResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> Resp c -> DHT m ()
routeResponse cmd resp = askMessaging >>= \msgsys -> liftDHT $ _routeResponse msgsys cmd resp

-- send a bytestring to an address
sendBytes :: (Monad m,Functor m) => Addr -> ByteString -> DHT m ()
sendBytes tAddr bs = askMessaging >>= \msgsys -> liftDHT $ _sendBytes msgsys tAddr bs

-- Send a (request/ response) Message, waiting for a response if we sent a request
-- and returning immediately if we sent a response.
sendMessage :: (Monad m,Functor m
               ,Binary (In c)
               ,Binary (Resp c)
               ,Typeable r
               )
            => Addr -> Message t r c -> DHT m r
sendMessage tAddr msg = do
  insertAddr tAddr
  let msgBs = encodeMessage msg
  case msg of
    -- Responses are simply sent
    ResponseMsg _ _
      -> sendBytes tAddr msgBs

    -- Requests are sent but we wait for a response
    RequestMsg cmd i
      -> do sendBytes tAddr msgBs
            waitResponse cmd i


-- insert a contact address into the routing table
-- LOCAL
insertAddr :: (Monad m,Functor m) => Addr -> DHT m ()
insertAddr cAddr = do
  now          <- timeNow
  routingTable <- askRoutingTable
  _rtInsert routingTable cAddr now (\addr -> ping addr >> return True)

-- insert multiple contact addresses into the routing table, all at the same time
-- LOCAL
insertAddrs :: (Monad m,Functor m) => [Addr] -> DHT m ()
insertAddrs cAddrs = do
  now          <- timeNow
  routingTable <- askRoutingTable
  let rtInsert = _rtInsert routingTable
  mapM_ (\cAddr -> rtInsert cAddr now (\addr -> ping addr >> return True)) cAddrs

-- attempt lookup of a contact with the given ID. Also return the k closest Contacts
-- LOCAL
lookupContact :: (Monad m,Functor m) => Addr -> ID -> DHT m ([Contact],Maybe Contact)
lookupContact enquirerAddr targetID = do
  now          <- timeNow
  routingTable <- askRoutingTable
  let rtLookup = _rtLookup routingTable
  liftDHT $ rtLookup enquirerAddr targetID now


-- | The k size is:
-- - The number of IDs stored per bucket in the routing table
-- - The number of neighbour contacts returned from queries if possible
-- Currently (undesirably...) doubles as the number of bits in ID's.
kSize :: (Monad m,Functor m) => DHT m Int
kSize = askRoutingTable >>= liftDHT . _rtKSize


-- insert a value into the local storage ONLY
insertValue :: (Monad m,Functor m) => ID -> ByteString -> DHT m ()
insertValue vID v = askValueStore >>= \valStore -> liftDHT $ _valInsert valStore vID v

-- lookup a value from the local storage ONLY
lookupValue :: (Monad m,Functor m) => ID -> DHT m (Maybe ByteString)
lookupValue vID = askValueStore >>= \valStore -> liftDHT $ _valLookup valStore vID


-- | Log a string if the logging system is enabled.
lg :: (Monad m,Functor m) => String -> DHT m ()
lg s = askLogging >>= liftDHT . maybe (return ()) ($ s)



{- DHT Operations on the networked DHT -}

-- | Send a ping to an 'Addr'ess.
ping :: (Monad m,Functor m) => Addr -> DHT m ()
ping tAddr = randomInt >>= \i -> pingThis i tAddr

-- | Send a ping to all the 'Addr'esses
pingAll :: (Monad m,Functor m) => [Addr] -> DHT m ()
pingAll = mapM_ ping

-- | Send a ping with a specific Int to an 'Addr'ess.
pingThis :: (Monad m,Functor m) => Int -> Addr -> DHT m ()
pingThis i tAddr = do
  j <- sendMessage tAddr $ PingRequestMsg i
  unless (i == j) invalidResponse

-- | Store a ByteString value at the appropriate place(s) in the DHT.
store :: (Monad m,Functor m) => ByteString -> DHT m ID
store v = do
  size    <- kSize
  ourAddr <- askOurAddr
  let vID = mkID v size
  res     <- lookupContact ourAddr vID
  let cs  = case res of
              (cs,Just c)  -> c:cs
              (cs,Nothing) -> cs
  ids <- mapM (storeAt v . _addr) cs

  -- TODO: Although its likely the messaging system wont even return to us non-matching ID's
  -- it is permitted to act that way. Therefore we should check that all ID's are the same (and
  -- correct). Bad contacts should then be marked.
  --
  -- TODO: We may have an empty list of targets to store at, perhaps it should be indicated/ an error
  -- if we don't even attempt to store anything?
  return vID

-- Store a ByteString value at the given 'Addr'ess.
storeAt :: (Monad m,Functor m) => ByteString -> Addr -> DHT m ID
storeAt v tAddr = do
  vID <- sendMessage tAddr $ StoreRequestMsg v

  -- TODO: Cache where we stored the value for faster retrieval?
  insertAddr tAddr
  return vID

-- | Attempt to find the 'Contact' with the given 'ID' alongwith a list of the
-- closest neighbour Contacts. Searches the global DHT if necessary.
findContact :: (Monad m,Functor m) => ID -> DHT m ([Contact],Maybe Contact)
findContact cID = do
  ourAddr <- askOurAddr
  res     <- lookupContact ourAddr cID
  case res of

    -- TODO: If we found it but don't know enough neighbours, maybe we should try and get more from
    -- the global DHT?
    (_,Just _) -> return res
    (cs,_)     -> findData FindContact cID cs

-- | Attempt to find the ByteString value with the given 'ID' alongwith a list of the
-- closest neighbor Contacts. Searches the global DHT if necessary.
findValue :: (Monad m,Functor m) => ID -> DHT m ([Contact],Maybe ByteString)
findValue vID = do
  ourAddr <- askOurAddr
  res     <- lookupContact ourAddr vID
  let cs = case res of
             -- TODO: Same as 'findContact' TODO (which we could probably share more code with)
             (cs,Just c) -> c:cs
             (cs,_)      -> cs

  mV <- lookupValue vID
  case mV of
    Just v -> return (cs,Just v)
    _      -> findData FindValue vID cs

-- Attempt to find the data associated with sending some Command to Contacts in the DHT.
-- Return a list of neighbour Contacts alongside the possible found data.
findData :: (Monad m,Functor m
            ,([Contact],Maybe d) ~ Out c
            ,Binary (In c)
            ,Binary (Resp c)
            ,Typeable d
            )
         => Command c -> In c -> [Contact] -> DHT m (Out c)
findData cmd i cs = findData' cmd i [] cs
  where
    findData' cmd i askedCs considerCs = do

      -- Query each contact for their k-nearest known contacts, and the desired value if they have it
      results <- forM considerCs $
          \c -> sendMessage (_addr c) $ RequestMsg cmd i

      let result = flattenResults results
      case result of

        -- Sought data not found, only neighbouring contacts
        (cs,Nothing)
          | null (cs \\ askedCs) -> return result -- no new contacts to ask
          | otherwise            -> findData' cmd i (askedCs ++ considerCs) cs

        -- At least one contact returned the data
        _ -> return result

    -- pick the first result 'd' if present, flatten a list of unique 'l's.
    flattenResults :: Eq l => [([l],Maybe d)] -> ([l],Maybe d)
    flattenResults = first nub . foldr (\(xLs,xmr) (accLs,mr) -> (xLs ++ accLs, mplus mr xmr)) ([],Nothing)

-- | Execute a DHT program with the given configuration parameters.
runDHT :: (Monad m,Functor m)
       => Addr
       -> Int
       -> m Time
       -> m Int
       -> Messaging m
       -> RoutingTable m
       -> ValueStore m
       -> Logging m
       -> Addr
       -> DHT m a
       -> m (Either DHTError a)
runDHT ourAddr size timeF randF msgsys routingTable valStore logging bAddr dht =
  let ourID    = mkID ourAddr size
      config   = DHTConfig timeF randF msgsys routingTable valStore logging ourID ourAddr
      bDht     = bootstrap bAddr >> dht
     in _runDHT bDht config

-- | Bootstrap against a bootstrap address.
bootstrap :: (Monad m,Functor m) => Addr -> DHT m ()
bootstrap bAddr = do
  -- TODO: Maybe check they exist or respond to messages first?
  insertAddr bAddr

  -- ask for our neighbours
  ourID   <- askOurID
  ourAddr <- askOurAddr
  res     <- findContact ourID
  let cs = case res of
             (cs,Nothing) -> cs

             -- We're already known:
             (cs,Just c)
               -- Either:
               -- 1. Bootstrapping more than once.
               -- This is equivalent to pinging to let them know we still exist except we might also get
               -- knowledge of new neighbours.
               --
               -- 2. Havnt been forgotten since a previous session.
               -- We *should* be able to just continue where we left of
               | _addr c == ourAddr -> cs

               -- 3. Somebody elses ID has collided with us
               -- TODO: Do something reasonable other than nothing. Maybe change how IDs are picked.
               | otherwise -> []

  -- ping all of our neighbours so they know we exist and might enter their routing table.
  pingAll $ map _addr cs

-- | Terminate the connection to the DHT.
--
-- TODO: The configuration subsystems (messaging, routingtable,etc) might need cleanup actions to be ran
quitDHT :: Monad m => DHT m ()
quitDHT = return ()


-- | Handle an incoming message which has been sent to us.
handleMessage :: (Monad m,Functor m
                 ,Typeable (Out c)
                 )
              => Addr -> Message mt mr c -> DHT m ()
handleMessage enquirerAddr msg = case msg of

  -- Handle a request that we do something, and return the result
  RequestMsg cmd cmdInput -> case cmd of

    -- Echo the ping back
    Ping
      -> sendMessage enquirerAddr $ PingResponseMsg cmdInput

    -- Store the value in our hashmap and reply the values ID hash
    Store
      -> do k <- kSize
            let vID = mkID cmdInput k
            insertValue vID cmdInput
            sendMessage enquirerAddr $ StoreResponseMsg vID

    -- Lookup the requested contact ID locally
    FindContact
      -> do res <- lookupContact enquirerAddr cmdInput
            sendMessage enquirerAddr $ FindContactResponseMsg cmdInput res

    -- Lookup the requested value ID locally
    FindValue
      -> do mv <- lookupValue cmdInput

            -- k-closest contact near to the value
            res <- lookupContact enquirerAddr cmdInput

            let ns = case res of
                         (ns,Nothing) -> ns
                         (ns,Just n)  -> n:ns

            case mv of
              Just v  -> sendMessage enquirerAddr $ FindValueResponseMsg cmdInput (ns,Just v)
              Nothing -> sendMessage enquirerAddr $ FindValueResponseMsg cmdInput (ns,Nothing)

  -- A response to a query we probably made. 'routeResponse' is delegated to
  -- checking its actually in response to something we sent and routing it to wherever is waiting for the response
  ResponseMsg cmd r -> routeResponse cmd r

