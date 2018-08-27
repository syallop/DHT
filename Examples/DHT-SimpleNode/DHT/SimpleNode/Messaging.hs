{-# lANGUAGE DataKinds, TypeFamilies, UndecidableInstances, DeriveDataTypeable, FlexibleContexts, GADTs, KindSignatures, RankNTypes, ScopedTypeVariables #-}
{-|
Stability : experimental
 -}
module DHT.SimpleNode.Messaging
  ( newSimpleMessaging
  )
  where

import DHT
import DHT.Command
import DHT.Contact
import DHT.Types

import Control.Concurrent
import Data.Typeable

import qualified Data.ByteString.Char8          as Strict
import qualified Data.ByteString.Lazy.Char8     as Lazy
import qualified Network.Socket.ByteString      as Strict

import qualified Data.Map as Map

import Network.Socket

data MsgState = MsgState
  { -- Map response patterns waited for to MVars waiting for the corresponding message.
   _msgStateReplyMap   :: forall c. Map.Map RespPat (MVar SomeOut)

  -- Map each Address we've established contact with to its outgoing socket.
  ,_msgStateSocketsOut :: Map.Map Addr Socket

  -- Socket we're listening on, if it's created
  ,_msgStateSocketIn   :: Maybe Socket

  -- The amount of padding a port number is padded to.
  ,_msgStateMaxPortLength :: Int
  }
  -- TODO: These two state items need not be coupled together

newtype MessagingState = MessagingState (MVar MsgState)

modifyMessagingState :: MessagingState -> (MsgState -> IO (MsgState,a)) -> IO a
modifyMessagingState (MessagingState msgState) f = modifyMVar msgState f

-- | Create a new Messaging system which internally:
-- - Sends messages to addresses via UDP, reusing sockets and encoded such that
-- all messages have a preceding 'replyport' padded to the given maximum port length.
-- - Routes and waits on messages using MVars and a notion of pattern matching (rather than, for
-- example including 'unique' tokens in sent messages).
newSimpleMessaging :: Int -> (Int,Port) -> IO (MessagingOp IO)
newSimpleMessaging size (maxPortLength,ourPort) = do
  messagingState <- newMessagingState

  return $ mkMessaging messagingState
  where
    mkMessaging :: MessagingState -> MessagingOp IO
    mkMessaging msgState = MessagingOp (waitF msgState size)
                                       (routeF msgState)
                                       (sendF msgState (maxPortLength,ourPort))
                                       (recvF msgState)


    newMessagingState :: IO MessagingState
    newMessagingState = do m <- newMVar $ MsgState Map.empty Map.empty Nothing maxPortLength
                           return $ MessagingState m

-- Physically send bytes to the given address.
-- Reuse sockets if we've contacted the address before.
sendF :: MessagingState -> (Int, Port) -> Addr -> Lazy.ByteString -> IO ()
sendF messagingState (maxPortLength,ourPort) addr@(Addr ip port) bs = withSocketsDo $ modifyMessagingState messagingState $ \state -> do
  (sock,state') <- initialiseSocket state

  nBytes <- Strict.send sock (padPort ourPort `Strict.append` (Lazy.toStrict bs))

  -- TODO:
  {-if fromIntegral nBytes == (fromIntegral $ Lazy.length bs) + (fromIntegral $ Strict.length (padPort ourPort))-}
    {-then return ()-}
    {-else fail "SENT TOO LITTLE BYTES"-}

  return (state',())
  where
    -- If we dont know of an address, open a new socket and cache it, otherwise return the one in use.
    initialiseSocket state = do
      case Map.lookup addr $ _msgStateSocketsOut state of
          Just sock
            -> return (sock,state)

          Nothing
            -> do let ipv4     = AF_INET
                      udp      = 17
                      udpPort  = fromInteger $ toInteger port
                  (inetAddr:_) <- getAddrInfo (Just defaultHints) (Just ip) (Just $ show port)

                  sock <- socket ipv4 Datagram udp
                  connect sock $ addrAddress inetAddr

                  setCloseOnExecIfNeeded $ fdSocket sock

                  return (sock, state{_msgStateSocketsOut = Map.insert addr sock $ _msgStateSocketsOut state})

    -- given a Port (represented by an Int), pad it with the appropriate number of leading zeros
    -- such that it is as long as the provided 'maxPortLength'
    padPort port =
      let portS   = show port
          lenPort = length portS
          lenPad  = maxPortLength - lenPort
          padding = replicate lenPad '0'
         in Strict.pack $ padding ++ portS

-- Wait for the response to a Command that has been sent with some input and return the response
-- when recieved from 'routeF'.
waitF :: MessagingState -> Int -> WaitF IO
waitF messagingState size cmd input = do
  waitMVar <- modifyMessagingState messagingState $ \msgState -> do
    let state@(MsgState replyMap _ _ _) = msgState
    waitMVar :: MVar SomeOut <- newEmptyMVar
    let pattern   = respPat size cmd input
        replyMap' = Map.insert pattern waitMVar replyMap
    return (state{_msgStateReplyMap = replyMap'},waitMVar)

  (SomeOut cmd o) <- takeMVar waitMVar
  case cast o of
      Nothing -> error "Invalid cast"
      Just o' -> return (o')

-- Route a response to a command to where it is being waited for.
routeF :: MessagingState -> RouteF IO
routeF messagingState cmd resp = do
  mRespToPut <- modifyMessagingState messagingState $ \msgState -> do
      let state@(MsgState replyMap _ _ _) = msgState
          (pattern,out) = disassemble cmd resp

      case Map.lookup pattern replyMap of

          -- TODO: We've been passed a response to something that isnt being waited on.
          -- Perhaps this should be indicated instead of just silently dropped.
          Nothing
            -> return (state{_msgStateReplyMap=replyMap},Nothing)

          Just waitMVar
            -> do let replyMap' = Map.delete pattern replyMap
                  return (state{_msgStateReplyMap=replyMap'},Just (waitMVar,(SomeOut cmd out)))

  case mRespToPut of
    Nothing
      -> return ()

    Just (waitMVar,someOut)
      -> putMVar waitMVar someOut

-- Receive a message from a sending DHT. Either a request or a response to a
-- request we made.
recvF :: MessagingState -> RecvF IO
recvF (MessagingState msgState) (Addr ourIP ourPort) = do
  state <- takeMVar msgState
  case _msgStateSocketIn state of
    -- We havnt initialised our incoming socket
    Nothing
      -> do (ourSock,state') <- openOurSocket state
            putMVar msgState state'
            recvF' ourSock state

    Just ourSock
      -> do putMVar msgState state
            recvF' ourSock state
  where
    recvF' ourSock state = do
      (msg,fromAddr) <- Strict.recvFrom ourSock 10240
      let (replyPortMsg,msg') = Strict.splitAt (_msgStateMaxPortLength state) msg
      (Just fromHostname, _) <- getNameInfo [] True True fromAddr
      let replyPort = toPort replyPortMsg
          replyAddr = Addr fromHostname replyPort

      return (replyAddr,Lazy.fromStrict msg')


    openOurSocket state = do
      ourSock <- socket AF_INET Datagram 17
      let udpPort = fromInteger $ toInteger ourPort
      inetAddr:_ <- getAddrInfo (Just defaultHints) (Just ourIP) (Just $ show ourPort)
      bind ourSock $ addrAddress inetAddr
      return (ourSock,state{_msgStateSocketIn = Just ourSock})

    toPort :: Strict.ByteString -> Port
    toPort = read . Strict.unpack

data SomeOut = forall c. Typeable (Out c) => SomeOut (Command c) (Out c)
  deriving Typeable

-- The pattern we look for to match a sent Command.
data RespPat = forall c. (Ord (Out c), Ord (In c)) => RespPat (Command c) (RespPatT c)
type family RespPatT (cmd :: CMD) where
  RespPatT PING        = Out PING
  RespPatT STORE       = Out STORE
  RespPatT FINDCONTACT = In FINDCONTACT
  RespPatT FINDVALUE   = In FINDVALUE

instance Ord RespPat where
  compare = compareRespPat

compareRespPat :: RespPat -> RespPat -> Ordering
compareRespPat (RespPat cmda pata) (RespPat cmdb patb) = case (cmda,cmdb) of

  -- witness the commands are the same so we know the pats are the same and can compare them
  -- When not the same, ordering Ping,Store,FindContact,FindValue
  (Ping       ,Ping)        -> compare pata patb
  (Ping       ,_)           -> LT
  (_          ,Ping)        -> GT

  (Store      ,Store)       -> compare pata patb
  (Store      ,_)           -> LT
  (_          ,Store)       -> GT

  (FindContact,FindContact) -> compare pata patb
  (FindContact,_)           -> LT
  (_          ,FindContact) -> GT

  (FindValue  ,FindValue)   -> compare pata patb

instance Eq RespPat where
  (==) = eqRespPat

eqRespPat :: RespPat -> RespPat -> Bool
eqRespPat (RespPat cmda pata) (RespPat cmdb patb) = case (cmda,cmdb) of

  (Ping        , Ping)        -> pata == patb
  (Store       , Store)       -> pata == patb
  (FindContact , FindContact) -> pata == patb
  (FindValue   , FindValue)   -> pata == patb
  (_           , _)           -> False

-- given a sent command and its input, create a response pattern to wait for
respPat :: Int -> Command c -> In c -> RespPat
respPat size cmd input = case (cmd,input) of
  (Ping,i)
    -> RespPat cmd i

  (Store,(keyId,valBs))
    -> RespPat cmd keyId

  (FindValue, keyID)
    -> RespPat cmd keyID

  (FindContact, nID)
    -> RespPat cmd nID

-- given a response to a command, dissassemble it into the response pattern it matches and the output it provides
disassemble :: Command c -> Resp c -> (RespPat,Out c)
disassemble cmd resp = case (cmd,resp) of
  (Ping,i)
    -> (RespPat cmd i,i)

  (Store,keyID)
    -> (RespPat cmd keyID,keyID)

  (FindValue, (vID,res))
    -> (RespPat cmd vID,res)

  (FindContact , (nID,res))
    -> (RespPat cmd nID,res)

