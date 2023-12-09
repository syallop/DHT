{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  , TypeOperators
  #-}
{-|
Stability : experimental

Model a DHT and its common operations leaving configuration 'holes' for
dependencies ('Messaging','RoutingTable','ValueStore','Logging') which are fulfilled
when the final DHT program is executed with 'runDHT'.
 -}
module DHT
  (-- * DHT Computations
   --
    DHT ()
  , DHTError (..)

  -- * Configuration
  , Config ()
  , mkConfig
  , ourID
  , ourAddress
  , hashSize
  , bootstrapAddress
  , kSize

  -- * DHT lifecycle
  --
  -- Operations that you will want to call to manage the DHT's lifecycle, I.E.
  -- starting interaction with remote clients, bootstrapping and quitting.
  , runDHT

  -- ** Start
  --
  -- In order for the DHT to interact with other remote instances, you must
  -- either:
  -- - Call 'startMessaging' (probably asynchronously).
  -- - Manually pump recvAndHandleMessage
  -- - Extra-manually use 'decodeSomeMessage' and pass the result into
  -- 'handleMessage'.
  , startMessaging

  , handleMessage
  , recvAndHandleMessage
  , recvAndHandleMessages

  -- ** Bootstrap
  , bootstrap
  , bootstrapFrom

  -- ** Quit
  , quitDHT

  -- * Core Operations
  -- ** For remote clients
  , ping, pingAll, pingThis
  , store
  , findValue
  , findContact

  -- ** For local client
  , lookupValue
  , lookupContact

  -- * Other Operations
  , lg
  , currentTime
  , randomInt
  , joinDHT
  , liftDHT

  -- * Errors
  , dhtError
  , timeOut
  , invalidResponse
  )
  where

import Control.Arrow              (first)
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List                  ((\\),nub,intercalate)
import Data.Typeable

import DHT.Core.Address
import DHT.Core.Contact
import DHT.Core.ID
import DHT.Core.Types
import DHT.Protocol.Command
import DHT.Protocol.Message

import qualified DHT.Client.Op as Op

-- TODO:
-- - In general there are too many layers of indirection between the Ops and the
-- top level DHT functions.
-- - Must startMessaging be separate from runDHT?

{- DHT configuration components -}

-- | DHT configuration is threaded through DHT computations and provides access
-- to static configuration, including injected 'Op'erational behavior.
data Config dht m = Config
  { _configOp               :: Op.Op dht m   -- ^ The operations required to run the DHT.
  , _configOurAddress       :: Address       -- ^ Our Address
  , _configOurID            :: ID            -- ^ The ID we calculate for ourself.
  , _configHashSize         :: Int           -- ^ How many bits to use in ID's.
  , _configBootstrapAddress :: Maybe Address -- ^ Address to bootstrap from.
  }

-- | Create the configuration required to run a DHT computation.
--
-- Requires concrete implementation choices via 'Op' that describe Routing,
-- Messaging, Storing, etc.
--
-- The Address and bit size will be used to compute our ID.
mkConfig
  :: Op.Op dht m    -- ^ The 'Op'erations required to run the DHT (messaging,routing,etc).
  -> Address        -- ^ Our Address.
  -> Int            -- ^ How many bits to use in IDs.
  -> Maybe Address  -- ^ Address to bootstrap from.
  -> Config dht m
mkConfig op addr hashSizeBits bootstrapAddr = Config
  { _configOp               = op
  , _configOurAddress       = addr
  , _configOurID            = mkID addr hashSizeBits
  , _configHashSize         = hashSizeBits
  , _configBootstrapAddress = bootstrapAddr
  }

{- The DHT type, it's instances and public, primitive operations -}

-- | DHT errors that may be returned from a DHT computation.
data DHTError
  = ETimeOut         -- ^ Timed out waiting for a response
  | EInvalidResponse -- ^ Invalid response

-- | A computation in the context of some DHTConfig executes in 'm' and either
-- shortcircuits to a DHTError (with the monad instance) or returns an 'a'.
newtype DHT m a = DHT {_runDHT :: Config DHT m -> m (Either DHTError a)}

-- DHTs Monad instance threads DHTState as readable state, executes in 'm' and
-- shortcircuits if a DHTError is returned.
instance Monad m => Monad (DHT m) where
  (DHT ma) >>= f = DHT $
    \r -> do res <- ma r
             case res of
               Left err -> return $ Left err
               Right a  -> _runDHT (f a) r

instance (Monad m,Functor m) => Applicative (DHT m) where
  pure a = DHT $ \_ -> pure $ Right a
  (<*>) = ap

instance Functor m => Functor (DHT m) where
  fmap f (DHT d) = DHT $ \r -> fmap f <$> d r

-- | Lift an action in 'm' into DHT.
liftDHT :: Functor m => m a -> DHT m a
liftDHT = DHT . const . (Right <$>)

-- | Join an 'm a' inside a DHT to the outer context.
joinDHT :: (Monad m,Functor m) => DHT m (m a) -> DHT m a
joinDHT = (>>= liftDHT)


{- Internal helpers for writing DHT functions -}

-- End a DHT with a DHTError.
dhtError :: Applicative m => DHTError -> DHT m a
dhtError = DHT . const . pure . Left

-- End a DHT with a ETimeOut.
timeOut :: Applicative m => DHT m a
timeOut = dhtError ETimeOut

-- End a DHT with an EInvalidResponse.
invalidResponse :: Monad m => DHT m a
invalidResponse = dhtError EInvalidResponse


{- Public accessors of config -}

-- | Our own ID.
ourID :: Applicative m => DHT m ID
ourID = DHT $ pure . Right . _configOurID

-- | Our own address.
ourAddress :: Applicative m => DHT m Address
ourAddress = DHT $ pure . Right . _configOurAddress

-- | The Hash Size used for keys.
hashSize :: Applicative m => DHT m Int
hashSize = DHT $ pure . Right . _configHashSize

-- | A Possible bootstrap address.
bootstrapAddress :: Applicative m => DHT m (Maybe Address)
bootstrapAddress = DHT $ pure . Right . _configBootstrapAddress

-- | The k size is:
-- - The number of IDs stored per bucket in the routing table
-- - The number of neighbour contacts returned from queries if possible
-- Currently (undesirably...) doubles as the number of bits in ID's.
kSize :: (Monad m,Functor m) => DHT m Int
kSize = DHT $ fmap Right . Op.kSize . _configOp

{- Public operations -}

-- | Log a string if the logging system is enabled.
lg :: (Monad m,Functor m) => String -> DHT m ()
lg s = DHT $ fmap Right . Op.withLogging (maybe (pure ()) ($ s)) . _configOp

-- | The current time
currentTime :: (Monad m,Functor m) => DHT m Time
currentTime = DHT $ fmap Right . Op.currentTime . _configOp

-- | A random Int
randomInt :: (Monad m,Functor m) => DHT m Int
randomInt = DHT $ fmap Right . Op.randomInt . _configOp


{- DHT Operations on the networked DHT -}

-- | Send a ping to an Address.
ping :: (Monad m,Functor m) => Address -> DHT m ()
ping targetAddress = randomInt >>= \i -> pingThis i targetAddress

-- | Send a ping to all the 'Address'es
pingAll :: (Monad m,Functor m) => [Address] -> DHT m ()
pingAll = mapM_ ping

-- | Send a ping with a specific Int to an 'Address'.
pingThis :: (Monad m,Functor m) => Int -> Address -> DHT m ()
pingThis i targetAddress = do
  j <- sendMessage targetAddress $ PingRequestMsg i
  unless (i == j) invalidResponse

-- | Store a ByteString value at the appropriate place(s) in the DHT.
store :: (Monad m,Functor m) => ByteString -> ByteString -> DHT m ID
store key val = do
  keyID   <- mkID <$> pure key <*> hashSize
  ourAddr <- ourAddress
  res     <- lookupContact ourAddr keyID
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

  pure keyID

-- Store a ByteString value at the given 'Addr'ess.
storeAt :: (Monad m,Functor m) => ID -> ByteString -> Address -> DHT m ID
storeAt keyID val targetAddress = do
  -- actualKeyID should be equal to the keyId we requested storage at
  let msg = StoreRequestMsg keyID val
  actualKeyID <- sendMessage targetAddress msg

  -- TODO: Cache where we stored the value for faster retrieval?
  insertAddress targetAddress
  return actualKeyID

-- | Attempt to find the 'Contact' with the given 'ID' alongwith a list of the
-- closest neighbour Contacts. Searches the global DHT if necessary.
findContact :: (Monad m,Functor m) => ID -> DHT m ([Contact],Maybe Contact)
findContact cID = do
  ourAddr <- ourAddress
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
  ourAddr <- ourAddress
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


{- DHT operations on local client -}

-- | Attempt lookup of a contact with the given ID. Also return the k closest Contacts
-- LOCAL
lookupContact :: (Monad m,Functor m) => Address -> ID -> DHT m ([Contact],Maybe Contact)
lookupContact enquirerAddr targetID = DHT $ \config -> do
  now <- Op.currentTime (_configOp config)
  Right <$> Op.lookupContact (_configOp config) enquirerAddr targetID now

-- | Lookup a value from the local storage ONLY
lookupValue :: (Monad m,Functor m) => ID -> DHT m (Maybe ByteString)
lookupValue keyID = DHT $ fmap Right . (\op -> Op.lookupIDValue op keyID) . _configOp

-- TODO: Any reason not to export the following functions on the local state?

-- insert a contact address into the routing table
-- LOCAL
insertAddress :: (Monad m, Functor m) => Address -> DHT m ()
insertAddress contact = DHT $ \config -> do
  let op = _configOp config
  now <- Op.currentTime op

  let DHT f = Op.insertAddress op contact now (\addr -> ping addr >> pure True)
  f config

-- insert a value into the local storage ONLY
insertValue :: (Monad m, Functor m) => ID -> ByteString -> DHT m ()
insertValue keyID val = DHT $ \config -> do
  let op = _configOp config
  Right <$> Op.insertIDValue op keyID val



{- DHT Lifecycle functions -}

-- | Execute a DHT program with the given configuration parameters.
--
-- You will likely wish to call 'DHT Lifecycle' functions in appropriate places.
-- E.G.
-- - 'startMessaging' before/ asynchronously.
-- - 'bootstrap' as an initial operation in the DHT.
-- - 'quitDHT' as a final operation in the DHT.
runDHT :: Monad m
       => Config DHT m
       -> DHT m a
       -> m (Either DHTError a)
runDHT config program = _runDHT program config

-- | For the DHT to actually send and receive messages, you probably want to run
-- this in the background E.G. with a forkIO.
-- Alternatively:
-- - Run 'recvAndHandleMessages' with your Config whenever appropriate.
-- - Manually pump recvAndHandleMessage
-- - Extra manually use 'decodeSomeMessage' and pass the result into
-- 'handleMessage'.
startMessaging
  :: Monad m
  => Config DHT m
  -> m (Either DHTError ())
startMessaging dhtConfig = runDHT dhtConfig recvAndHandleMessages

-- | Bootstrap against the DHTs configured bootstrap address.
bootstrap :: Monad m => DHT m ()
bootstrap = bootstrapAddress >>= \addr -> case addr of
  Nothing
    -> lg "Bootstrapping without a configured address will do nothing."

  Just bootstrapAddr
    -> bootstrapFrom bootstrapAddr

-- | Bootstrap against a bootstrap address.
bootstrapFrom :: (Monad m,Functor m) => Address -> DHT m ()
bootstrapFrom bootstrapAddr = do
  -- TODO: Maybe check they exist or respond to messages first?
  insertAddress bootstrapAddr

  -- ask for our neighbours
  addr <- ourAddress
  res  <- ourID >>= findContact
  let cts = case res of
              (cs,Nothing)
                -> cs

              -- We're already known:
              (cs,Just c)
                -- Either:
                -- 1. Bootstrapping more than once.
                -- This is equivalent to pinging to let them know we still exist except we might also get
                -- knowledge of new neighbours.
                --
                -- 2. Havnt been forgotten since a previous session.
                -- We *should* be able to just continue where we left of
                | contactAddress c == addr
                 -> cs

                -- 3. Somebody elses ID has collided with us
                -- TODO: Do something reasonable other than nothing. Maybe change how IDs are picked.
                | otherwise
                 -> []

  -- ping all of our neighbours so they know we exist and might enter their routing table.
  pingAll $ map contactAddress cts

-- | Terminate the connection to the DHT.
--
-- TODO: The configuration subsystems (messaging, routingtable,etc) might need cleanup actions to be ran
quitDHT :: Monad m => DHT m ()
quitDHT = return ()

-- | Handle an incoming message which has been sent to us.
-- The sender is potentially added to our routing table.
handleMessage
  :: ( Monad m
     , Functor m
     , Typeable (Out c)
     , Show (Resp c)
     )
  => Address -> Message mt mr c -> DHT m ()
handleMessage enquirerAddress msg = do
  insertAddress enquirerAddress
  case msg of

    -- Handle a request that we do something, and return the result
    RequestMsg cmd cmdInput -> case cmd of

      -- Echo the ping back
      Ping
        -> sendMessage enquirerAddress $ PingResponseMsg cmdInput

      -- Store the value in our hashmap and reply the values ID hash
      Store
        -> do let (keyID,val) = cmdInput
              insertValue keyID val
              sendMessage enquirerAddress $ StoreResponseMsg keyID

      -- Lookup the requested contact ID locally
      FindContact
        -> do res <- lookupContact enquirerAddress cmdInput
              sendMessage enquirerAddress $ FindContactResponseMsg cmdInput res

      -- Lookup the requested value ID locally
      FindValue
        -> do mv <- lookupValue cmdInput

              -- k-closest contact near to the value
              res <- lookupContact enquirerAddress cmdInput

              let nbs = case res of
                          (ns,Nothing) -> ns
                          (ns,Just n)  -> n:ns

              case mv of
                Just v  -> sendMessage enquirerAddress $ FindValueResponseMsg cmdInput (nbs,Just v)
                Nothing -> sendMessage enquirerAddress $ FindValueResponseMsg cmdInput (nbs,Nothing)

    -- A response to a query we probably made. 'routeResponse' is delegated to
    -- checking its actually in response to something we sent and routing it to wherever is waiting for the response
    ResponseMsg cmd r
      -> routeResponse cmd r

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
recvAndHandleMessages = forever recvAndHandleMessage


{- Internal/ networking operations -}

-- wait for a response to a command
waitResponse
  :: ( Functor m
     , Typeable (Out c)
     )
  => Command c -> In c -> DHT m (Out c)
waitResponse cmd input = DHT $ fmap Right . (\op -> Op.wait op cmd input). _configOp

-- route a response to the correct waiing location
routeResponse
  :: ( Functor m
     , Typeable (Out c)
     )
  => Command c -> Resp c -> DHT m ()
routeResponse cmd resp = DHT $ fmap Right . (\op -> Op.route op cmd resp) . _configOp

-- send a bytestring to an address
sendBytes :: Functor m => Address -> ByteString -> DHT m ()
sendBytes targetAddress bs = DHT $ fmap Right . (\op -> Op.sendBytes op targetAddress bs) . _configOp

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
recvBytes = DHT $ \config -> fmap Right $ Op.recvBytes (_configOp config) (_configOurAddress config)

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

