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
  ,DHTOp(..)
  ,DHTConfig(..)
  ,DHTState()
  ,MessagingOp(..)
  ,RoutingTableOp(..)
  ,ValueStoreOp(..)
  ,LoggingOp
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

  ,joinDHT
  ,timeOut
  )
  where

import Control.Arrow              (first)
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List                  ((\\),nub)
import Data.Typeable

import DHT.Command
import DHT.Contact
import DHT.ID
import DHT.Message
import DHT.Types

import DHT.Op

{- DHT configuration components -}
-- | DHT configuration
data DHTConfig dht m = DHTConfig
  {_dhtConfigOps  :: DHTOp dht m -- ^ The operations we require to implement the DHT
  ,_dhtConfigAddr :: Addr        -- ^ Our address
  }

-- |
data DHTState dht m = DHTState
  {_dhtStateConfig :: DHTConfig dht m -- ^ User configuration contains injected subsystems
  ,_dhtStateID     :: ID              -- ^ The ID we calculate for ourself
  }

{- The DHT type, instances and primitive operations -}

-- | DHT errors
data DHTError
  = ETimeOut         -- ^ Timed out waiting for a response
  | EInvalidResponse -- ^ Invalid response

-- | A computation in the context of some DHTConfig executes in 'm' and either
-- shortcircuits to a DHTError (with the monad instance) or returns an 'a'.
newtype DHT m a = DHT {_runDHT :: DHTState DHT m -> m (Either DHTError a)}

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
ask :: Monad m => DHT m (DHTConfig DHT m)
ask = DHT (succeed . _dhtStateConfig)

-- | Our own ID
askOurID :: Monad m => DHT m ID
askOurID = DHT (succeed . _dhtStateID)

-- | Our own address
askOurAddr :: Monad m => DHT m Addr
askOurAddr = _dhtConfigAddr <$> ask

-- ask for the messaging system
askMessagingOp :: (Monad m,Functor m) => DHT m (MessagingOp m)
askMessagingOp = _dhtOpMessagingOp . _dhtConfigOps <$> ask

-- ask for the routing table system
askRoutingTableOp :: (Monad m,Functor m) => DHT m (RoutingTableOp DHT m)
askRoutingTableOp = _dhtOpRoutingTableOp . _dhtConfigOps <$> ask

-- ask for the value storage systen
askValueStoreOp :: (Monad m,Functor m) => DHT m (ValueStoreOp m)
askValueStoreOp = _dhtOpValueStoreOp . _dhtConfigOps <$> ask

-- ask for the logging system
askLoggingOp :: (Monad m,Functor m) => DHT m (LoggingOp m)
askLoggingOp = _dhtOpLoggingOp . _dhtConfigOps <$> ask


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
timeNow = ask >>= liftDHT . _dhtOpTimeOp . _dhtConfigOps

-- | A random Int
randomInt :: (Monad m,Functor m) => DHT m Int
randomInt = ask >>= liftDHT . _dhtOpRandomIntOp . _dhtConfigOps


-- wait for a response to a command
waitResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> In c -> DHT m (Out c)
waitResponse cmd i = askMessagingOp >>= \msgsys -> liftDHT $ _messagingOpWaitResponse msgsys cmd i

-- route a response to the correct waiing location
routeResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> Resp c -> DHT m ()
routeResponse cmd resp = askMessagingOp >>= \msgsys -> liftDHT $ _messagingOpRouteResponse msgsys cmd resp

-- send a bytestring to an address
sendBytes :: (Monad m,Functor m) => Addr -> ByteString -> DHT m ()
sendBytes tAddr bs = askMessagingOp >>= \msgsys -> liftDHT $ _messagingOpSendBytes msgsys tAddr bs

-- Send a (request/ response) Message, waiting for a response if we sent a request
-- and returning immediately if we sent a response.
sendMessage :: (Monad m,Functor m
               ,Binary (In c)
               ,Binary (Resp c)
               ,Typeable r
               )
            => Addr -> Message t r c -> DHT m r
sendMessage tAddr msg = do
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
  routingTable <- askRoutingTableOp
  _routingTableOpInsert routingTable cAddr now (\addr -> ping addr >> return True)

-- insert multiple contact addresses into the routing table, all at the same time
-- LOCAL
insertAddrs :: (Monad m,Functor m) => [Addr] -> DHT m ()
insertAddrs cAddrs = do
  now          <- timeNow
  routingTable <- askRoutingTableOp
  let rtInsert = _routingTableOpInsert routingTable
  mapM_ (\cAddr -> rtInsert cAddr now (\addr -> ping addr >> return True)) cAddrs

-- attempt lookup of a contact with the given ID. Also return the k closest Contacts
-- LOCAL
lookupContact :: (Monad m,Functor m) => Addr -> ID -> DHT m ([Contact],Maybe Contact)
lookupContact enquirerAddr targetID = do
  now          <- timeNow
  routingTable <- askRoutingTableOp
  let rtLookup = _routingTableOpLookup routingTable
  liftDHT $ rtLookup enquirerAddr targetID now


-- | The k size is:
-- - The number of IDs stored per bucket in the routing table
-- - The number of neighbour contacts returned from queries if possible
-- Currently (undesirably...) doubles as the number of bits in ID's.
kSize :: (Monad m,Functor m) => DHT m Int
kSize = askRoutingTableOp >>= liftDHT . _routingTableOpKSize


-- insert a value into the local storage ONLY
insertValue :: (Monad m,Functor m) => ID -> ByteString -> DHT m ()
insertValue keyID val = askValueStoreOp >>= \valStore -> liftDHT $ _valueStoreOpInsert valStore keyID val

-- lookup a value from the local storage ONLY
lookupValue :: (Monad m,Functor m) => ID -> DHT m (Maybe ByteString)
lookupValue keyID = askValueStoreOp >>= \valStore -> liftDHT $ _valueStoreOpLookup valStore keyID


-- | Log a string if the logging system is enabled.
lg :: (Monad m,Functor m) => String -> DHT m ()
lg s = askLoggingOp >>= liftDHT . maybe (return ()) ($ s)



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
store :: (Monad m,Functor m) => ByteString -> ByteString -> DHT m ID
store key val = do
  size    <- kSize
  ourAddr <- askOurAddr
  let keyID = mkID key size
  res     <- lookupContact ourAddr keyID
  let cts  = case res of
               (cs,Just c)  -> c:cs
               (cs,Nothing) -> cs
  _ids <- mapM (storeAt keyID val . _addr) cts

  -- TODO: Although its likely the messaging system wont even return to us non-matching ID's
  -- it is permitted to act that way. Therefore we should check that all ID's are the same (and
  -- correct). Bad contacts should then be marked.
  --
  -- TODO: We may have an empty list of targets to store at, perhaps it should be indicated/ an error
  -- if we don't even attempt to store anything?
  return keyID

-- Store a ByteString value at the given 'Addr'ess.
storeAt :: (Monad m,Functor m) => ID -> ByteString -> Addr -> DHT m ID
storeAt keyID val tAddr = do
  -- actualKeyID should be equal to the keyId we requested storage at
  let msg = StoreRequestMsg keyID val
  actualKeyID <- sendMessage tAddr msg

  -- TODO: Cache where we stored the value for faster retrieval?
  insertAddr tAddr
  return actualKeyID

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
  let cts = case res of
              -- TODO: Same as 'findContact' TODO (which we could probably share more code with)
              (cs,Just c) -> c:cs
              (cs,_)      -> cs

  mV <- lookupValue vID
  case mV of
    Just v -> return (cts,Just v)
    _      -> findData FindValue vID cts

-- Attempt to find the data associated with sending some Command to Contacts in the DHT.
-- Return a list of neighbour Contacts alongside the possible found data.
findData :: (Monad m,Functor m
            ,([Contact],Maybe d) ~ Out c
            ,Binary (In c)
            ,Binary (Resp c)
            ,Typeable d
            )
         => Command c -> In c -> [Contact] -> DHT m (Out c)
findData cmmnd input = findData' cmmnd input []
  where
    findData' cmnd i askedCs considerCs = do

      -- Query each contact for their k-nearest known contacts, and the desired value if they have it
      results <- forM considerCs $
          \c -> sendMessage (_addr c) $ RequestMsg cmnd i

      let result = flattenResults results
      case result of

        -- Sought data not found, only neighbouring contacts
        (cs,Nothing)
          | null (cs \\ askedCs) -> return result -- no new contacts to ask
          | otherwise            -> findData' cmnd i (askedCs ++ considerCs) cs

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
       -> MessagingOp m
       -> RoutingTableOp DHT m
       -> ValueStoreOp m
       -> LoggingOp m
       -> Maybe Addr
       -> DHT m a
       -> m (Either DHTError a)
runDHT ourAddr size timeF randF msgsys routingTable valStore logging mBootstrapAddr dht =
  let ops      = DHTOp timeF randF msgsys routingTable valStore logging
      config   = DHTConfig ops ourAddr
      ourID    = mkID ourAddr size
      state    = DHTState config ourID
      dht'     = case mBootstrapAddr of
                     Nothing -> dht
                     Just bootstrapAddr -> bootstrap bootstrapAddr >> dht
     in _runDHT dht' state

-- | Bootstrap against a bootstrap address.
bootstrap :: (Monad m,Functor m) => Addr -> DHT m ()
bootstrap bAddr = do
  -- TODO: Maybe check they exist or respond to messages first?
  insertAddr bAddr

  -- ask for our neighbours
  ourID   <- askOurID
  ourAddr <- askOurAddr
  res     <- findContact ourID
  let cts = case res of
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
  pingAll $ map _addr cts

-- | Terminate the connection to the DHT.
--
-- TODO: The configuration subsystems (messaging, routingtable,etc) might need cleanup actions to be ran
quitDHT :: Monad m => DHT m ()
quitDHT = return ()


-- | Handle an incoming message which has been sent to us.
-- The sender is potentially added to our routing table.
handleMessage :: (Monad m,Functor m
                 ,Typeable (Out c)
                 ,Show (Resp c)
                 )
              => Addr -> Message mt mr c -> DHT m ()
handleMessage enquirerAddr msg = do
  insertAddr enquirerAddr
  case msg of

    -- Handle a request that we do something, and return the result
    RequestMsg cmd cmdInput -> case cmd of

      -- Echo the ping back
      Ping
        -> sendMessage enquirerAddr $ PingResponseMsg cmdInput

      -- Store the value in our hashmap and reply the values ID hash
      Store
        -> do k <- kSize
              let (keyID,val) = cmdInput
              insertValue keyID val
              sendMessage enquirerAddr $ StoreResponseMsg keyID

      -- Lookup the requested contact ID locally
      FindContact
        -> do res <- lookupContact enquirerAddr cmdInput
              sendMessage enquirerAddr $ FindContactResponseMsg cmdInput res

      -- Lookup the requested value ID locally
      FindValue
        -> do mv <- lookupValue cmdInput

              -- k-closest contact near to the value
              res <- lookupContact enquirerAddr cmdInput

              let nbs = case res of
                          (ns,Nothing) -> ns
                          (ns,Just n)  -> n:ns

              case mv of
                Just v  -> sendMessage enquirerAddr $ FindValueResponseMsg cmdInput (nbs,Just v)
                Nothing -> sendMessage enquirerAddr $ FindValueResponseMsg cmdInput (nbs,Nothing)

    -- A response to a query we probably made. 'routeResponse' is delegated to
    -- checking its actually in response to something we sent and routing it to wherever is waiting for the response
    ResponseMsg cmd r
      -> routeResponse cmd r

