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
  (-- * DHT
   --
   -- Core DHT type in which computations are ran
    DHT()
  , DHTError(..)

  -- ** Configuration
  --
  -- These types and functions are used to initialise configuration for running
  -- DHT programs.
  , Op.Op()
  , DHTConfig(..)
  , DHTState()

  , Op.Messaging()
  , Op.mkMessaging

  , Op.RoutingTable()
  , Op.mkRoutingTable

  , Op.ValueStore()
  , Op.mkValueStore

  , Op.Logging
  , Op.SendF,Op.WaitF,Op.RouteF,Op.RecvF,Op.RTInsertF,Op.RTLookupF,Op.ValInsertF,Op.ValLookupF

  -- ** DHT lifecycle functions
  --
  -- Start, stop, DHT's.
  , runDHT
  , startMessaging
  , quitDHT

  , bootstrap
  , bootstrapFrom
  , liftDHT

  -- *** Core Operations
  --
  -- Primary interface for using the DHT as a client.
  , ping
  , store
  , findValue
  , findContact

  -- *** Query the local state
  , askOurID
  , askOurAddr
  , askHashSize
  , askBootstrapAddr
  , kSize
  , lookupValue
  , lookupContact

  -- *** Additional 'extra' operations, exposed for convenience
  , timeNow
  , randomInt
  , lg

  , joinDHT
  , timeOut

  -- *** Alternatives to calling 'startMessaging'
  , handleMessage
  , recvAndHandleMessage
  , recvAndHandleMessages
  )
  where

import Control.Arrow              (first)
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List                  ((\\),nub,intercalate)
import Data.Typeable

import DHT.Address
import DHT.Command
import DHT.Contact
import DHT.ID
import DHT.Message
import DHT.Types

import qualified DHT.Op as Op

-- TODO:
-- - Config shouldnt be in state, since it shouldnt be modified
-- - In fact, state doesnt get modified either so is there a reason not to
-- merge?
-- - In general there are too many layers of indirection between the Ops and the
-- top level DHT functions.

{- DHT configuration components -}
-- | DHT configuration
data DHTConfig dht m = DHTConfig
  { _dhtConfigOps           :: Op.Op dht m   -- ^ The operations we require to implement the DHT
  , _dhtConfigAddr          :: Address       -- ^ Our address
  , _dhtConfigHashSize      :: Int           -- ^ How many bits to use in hashed ID's
  , _dhtConfigBootstrapAddr :: Maybe Address -- ^ Address to bootstrap from
  }

-- | The DHTState is threaded through 'DHT' computations and contains the
-- initial DHTConfig and the ID we calculate for ourselves.
data DHTState dht m = DHTState
  {_dhtStateConfig :: DHTConfig dht m -- ^ User configuration contains injected subsystems
  ,_dhtStateID     :: ID              -- ^ The ID we calculate for ourself
  }

{- The DHT type, instances and primitive operations -}

-- | DHT errors that may be returned from a DHT computation.
data DHTError
  = ETimeOut         -- ^ Timed out waiting for a response
  | EInvalidResponse -- ^ Invalid response

-- | A computation in the context of some DHTConfig executes in 'm' and either
-- shortcircuits to a DHTError (with the monad instance) or returns an 'a'.
newtype DHT m a = DHT {_runDHT :: DHTState DHT m -> m (Either DHTError a)}

-- DHTs Monad instance threads DHTState as readable state, executes in 'm' and
-- shortcircuits if a DHTError is returned.
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

-- | Our own address.
askOurAddr :: Monad m => DHT m Address
askOurAddr = _dhtConfigAddr <$> ask

-- | The Hash Size used for keys.
askHashSize :: Monad m => DHT m Int
askHashSize = _dhtConfigHashSize <$> ask

-- | A Possible bootstrap address.
askBootstrapAddr :: Monad m => DHT m (Maybe Address)
askBootstrapAddr = _dhtConfigBootstrapAddr <$> ask

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
timeNow = ask >>= liftDHT . Op.currentTime . _dhtConfigOps

-- | A random Int
randomInt :: (Monad m,Functor m) => DHT m Int
randomInt = ask >>= liftDHT . Op.randomInt . _dhtConfigOps

-- wait for a response to a command
waitResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> In c -> DHT m (Out c)
waitResponse cmd i = ask >>= liftDHT . (\op -> Op.wait op cmd i) . _dhtConfigOps

-- route a response to the correct waiing location
routeResponse :: (Monad m,Functor m,Typeable (Out c)) => Command c -> Resp c -> DHT m ()
routeResponse cmd resp = ask >>= liftDHT . (\op -> Op.route op cmd resp) . _dhtConfigOps

-- send a bytestring to an address
sendBytes :: (Monad m,Functor m) => Address -> ByteString -> DHT m ()
sendBytes tAddr bs = ask >>= liftDHT . (\op -> Op.sendBytes op tAddr bs) . _dhtConfigOps

-- Send a (request/ response) Message, waiting for a response if we sent a request
-- and returning immediately if we sent a response.
sendMessage :: (Monad m,Functor m
               ,Binary (In c)
               ,Binary (Resp c)
               ,Typeable r
               )
            => Address -> Message t r c -> DHT m r
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


-- receive a bytestring on the DHTs address. Note the sender.
recvBytes :: Monad m => DHT m (Address,ByteString)
recvBytes = do
  addr   <- askOurAddr
  ask >>= liftDHT . (\op -> Op.recvBytes op addr) . _dhtConfigOps

-- receive SomeMessage on the DHTs address. Note the sender.
recvMessage :: Monad m => DHT m (Maybe (Address,SomeMessage))
recvMessage = do
  (sender,bs) <- recvBytes
  let mMsg = decodeSomeMessage bs
  case mMsg of
    -- Sender sent garbage
    Nothing
      -> return Nothing

    Just msg
      -> return $ Just (sender,msg)

-- receive and handle a single incoming message
recvAndHandleMessage :: Monad m => DHT m ()
recvAndHandleMessage = do
  mMsg <- recvMessage
  case mMsg of
    -- A received message hasnt parsed.
    -- TODO: Might want to note this somehow.
    Nothing
      -> return ()

    Just (sender,SomeMessage msg)
      -> handleMessage sender msg

-- | Receive and handle all incoming messages.
-- You must either:
-- - Call this
-- - Manually pump recvAndHandleMessage
-- - Extra manually use 'decodeSomeMessage' and pass the result into
-- 'handleMessage' for the DHT to receive responses to your requests and serve
-- requests from other DHTs.
recvAndHandleMessages :: Monad m => DHT m ()
recvAndHandleMessages = do
  recvAndHandleMessage
  recvAndHandleMessages

-- insert a contact address into the routing table
-- LOCAL
insertAddr :: (Monad m,Functor m) => Address -> DHT m ()
insertAddr cAddr = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state

  now <- Op.currentTime op
  let DHT f = Op.insertAddress op cAddr now (\addr -> ping addr >> pure True)
  f state

-- insert multiple contact addresses into the routing table, all at the same time
-- LOCAL
insertAddrs :: (Monad m,Functor m) => [Address] -> DHT m ()
insertAddrs cAddrs = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  now <- Op.currentTime op

  let DHT f = mapM_ (\cAddr -> Op.insertAddress op cAddr now (\addr -> ping addr >> pure True)) cAddrs
  f state

-- attempt lookup of a contact with the given ID. Also return the k closest Contacts
-- LOCAL
lookupContact :: (Monad m,Functor m) => Address -> ID -> DHT m ([Contact],Maybe Contact)
lookupContact enquirerAddr targetID = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  now <- Op.currentTime op

  Right <$> Op.lookupContact op enquirerAddr targetID now

-- | The k size is:
-- - The number of IDs stored per bucket in the routing table
-- - The number of neighbour contacts returned from queries if possible
-- Currently (undesirably...) doubles as the number of bits in ID's.
kSize :: (Monad m,Functor m) => DHT m Int
kSize = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  Right <$> Op.kSize op

-- insert a value into the local storage ONLY
insertValue :: (Monad m,Functor m) => ID -> ByteString -> DHT m ()
insertValue keyID val = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  Right <$> Op.insertIDValue op keyID val

-- lookup a value from the local storage ONLY
lookupValue :: (Monad m,Functor m) => ID -> DHT m (Maybe ByteString)
lookupValue keyID = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  Right <$> Op.lookupIDValue op keyID

-- | Log a string if the logging system is enabled.
lg :: (Monad m,Functor m) => String -> DHT m ()
lg s = DHT $ \state -> do
  let op = _dhtConfigOps . _dhtStateConfig $ state
  Right <$> Op.withLogging (maybe (pure ()) ($ s)) op


{- DHT Operations on the networked DHT -}

-- | Send a ping to an 'Addres's.
ping :: (Monad m,Functor m) => Address -> DHT m ()
ping tAddr = randomInt >>= \i -> pingThis i tAddr

-- | Send a ping to all the 'Address'es
pingAll :: (Monad m,Functor m) => [Address] -> DHT m ()
pingAll = mapM_ ping

-- | Send a ping with a specific Int to an 'Address'.
pingThis :: (Monad m,Functor m) => Int -> Address -> DHT m ()
pingThis i tAddr = do
  j <- sendMessage tAddr $ PingRequestMsg i
  unless (i == j) invalidResponse

-- | Store a ByteString value at the appropriate place(s) in the DHT.
store :: (Monad m,Functor m) => ByteString -> ByteString -> DHT m ID
store key val = do
  hashSize <- askHashSize
  ourAddr  <- askOurAddr
  let keyID = mkID key hashSize

  res <- lookupContact ourAddr keyID
  let cts  = case res of
               (cs,Just c)  -> c:cs
               (cs,Nothing) -> cs

  -- If there are no contacts to store at, store locally.
  --
  -- TODO: It's possible this should be a failure case.
  -- Or we should block waiting for contacts.
  -- Or the stored values should be queued to be stored later.
  --   Is/ can this be a responsibility of the value store?
  when (cts == [])
    $ do lg . mconcat
            $ ["WARN: Found no contacts to store key: "
              , show key
              , " with id "
              , show keyID
              , " so we're storing locally"
              ]
         insertValue keyID val

  -- TODO: If the messaging system has returned non-matching ID's in
  -- acknowledgment, we should mark the culprits as bad contacts/ surface an
  -- error.
  ids <- mapM (storeAt keyID val . contactAddress) cts
  let unexpectedIDs = filter (/= keyID) ids
  when (not . (== []) $ unexpectedIDs)
       $ lg . (("When storing id " <> show key <> " some contacts returned incorrect ids: ") <>)
            . intercalate ","
            . map show
            $ unexpectedIDs

  return keyID

-- Store a ByteString value at the given 'Addr'ess.
storeAt :: (Monad m,Functor m) => ID -> ByteString -> Address -> DHT m ID
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
          \c -> sendMessage (contactAddress c) $ RequestMsg cmnd i

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
runDHT :: Monad m
       => DHTConfig DHT m
       -> DHT m a
       -> m (Either DHTError a)
runDHT dhtConfig dhtProgram =
  let -- The state is the user config and an ID we're assigining to ourself
      -- based upon it.
      ourAddr    = _dhtConfigAddr dhtConfig
      hashSize   = _dhtConfigHashSize dhtConfig
      ourID      = mkID ourAddr hashSize
      dhtState   = DHTState dhtConfig ourID

     in _runDHT dhtProgram dhtState

-- | For the DHT to actually send and recieve messages, you probably want to run
-- this in the background E.G. with a forkIO.
-- Alternatively:
-- - Run 'recvAndHandleMessages' with your Config whenever appropriate.
-- - Manually pump recvAndHandleMessage
-- - Extra manually use 'decodeSomeMessage' and pass the result into
-- 'handleMessage'.
startMessaging
  :: Monad m
  => DHTConfig DHT m
  -> m (Either DHTError ())
startMessaging dhtConfig = runDHT dhtConfig recvAndHandleMessages

-- | Bootstrap against the DHTs configured bootstrap address.
bootstrap :: Monad m => DHT m ()
bootstrap = do
  config <- ask
  let mBootstrapAddr = _dhtConfigBootstrapAddr config
  case mBootstrapAddr of
    -- TODO: Log that we havnt?
    Nothing
      -> return ()

    Just bootstrapAddr
      -> bootstrapFrom bootstrapAddr

-- | Bootstrap against a bootstrap address.
bootstrapFrom :: (Monad m,Functor m) => Address -> DHT m ()
bootstrapFrom bAddr = do
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
                | contactAddress c == ourAddr -> cs

                -- 3. Somebody elses ID has collided with us
                -- TODO: Do something reasonable other than nothing. Maybe change how IDs are picked.
                | otherwise -> []

  -- ping all of our neighbours so they know we exist and might enter their routing table.
  pingAll $ map contactAddress cts

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
              => Address -> Message mt mr c -> DHT m ()
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

