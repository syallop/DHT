{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , PatternSynonyms
  , RankNTypes
  #-}
{-|
Stability : experimental

Encapsulate commands, their parameters and their response messages, able to be sent and recieved 'over the wire' via encoding to
ByteStrings with 'encodeMessage' and decoded to 'SomeMessage' via 'decodeSomeMessage'.
-}
module DHT.Message
  (-- * Core outwards message type
    Message (RequestMsg,ResponseMsg)

  -- ** Encoding
  , encodeMessage

  -- ** Specialised requests
  , pattern PingRequestMsg
  , pattern StoreRequestMsg
  , pattern FindContactRequestMsg
  , pattern FindContactAtRequestMsg
  , pattern FindValueRequestMsg
  , pattern FindValueAtRequestMsg

  -- ** Specialised responses
  , pattern PingResponseMsg
  , pattern StoreResponseMsg
  , pattern FindContactResponseMsg
  , pattern FindValueResponseMsg

  -- * Core inward message type
  , SomeMessage(SomeMessage)

  -- ** Decoding
  , decodeSomeMessage

  -- * Extra
  , showMessage
  ) where

import DHT.Address
import DHT.Command
import DHT.Contact

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Typeable
import Data.String.Conv

-- | A Message is a command either sent as a Request (along with its 'In'put, to a 'Target' and with an expected 'Out'put)
-- or a sent Response to some issued Request (along with a 'Resp' value, to an Addr and with no output)
--
-- Messages types tag:
--  - mt - the target of the message (Address,Either () Addr)
--  - mr - the response type (Out c/ ())
--  - c  - the command name
data Message mt mr c where
    RequestMsg  :: Command c -> In c
                -> Message (Target c) (Out c) c

    ResponseMsg :: Command c -> Resp c
                -> Message Address    ()      c

instance Show (Message mt mr c) where
  show = showMessage

showMessage :: Message mt mr c -> String
showMessage msg = case msg of
  RequestMsg cmd i -> "REQ " ++ show cmd ++ " " ++ case cmd of
    Ping        -> show i
    Store       -> let (keyId,msg) = i
                      in show keyId ++ ":" ++ toS msg
    FindContact -> show i
    FindValue   -> show i

  ResponseMsg cmd resp -> "RESP " ++ show cmd ++ case cmd of
    Ping        -> show resp
    Store       -> show resp
    FindContact -> let (cID,(cs,mc)) = resp in show cID ++ " " ++ showContacts cs ++ maybe "" (\c -> " " ++ showContact c) mc
    FindValue   -> let (vID,(cs,mv)) = resp in show vID ++ " " ++ showContacts cs ++ maybe "" show mv

-- | Encode a 'Message' to a ByteString.
encodeMessage :: (Binary (In c), Binary (Resp c)) => Message mt mr c -> ByteString
encodeMessage msg = runPut $ case msg of
    RequestMsg  c i -> putWord8 0 >> put (commandTag c) >> put i
    ResponseMsg c r -> putWord8 1 >> put (commandTag c) >> put r


-- request conveniences
pattern PingRequestMsg          i       = RequestMsg Ping        i
pattern StoreRequestMsg         kID val = RequestMsg Store (kID,val)
pattern FindContactRequestMsg   cID     = RequestMsg FindContact cID
pattern FindContactAtRequestMsg cID     = RequestMsg FindContact cID
pattern FindValueRequestMsg     vID     = RequestMsg FindValue   vID
pattern FindValueAtRequestMsg   vID     = RequestMsg FindValue   vID

-- response conveniences
pattern PingResponseMsg        i       = ResponseMsg Ping        i
pattern StoreResponseMsg       vID     = ResponseMsg Store       vID
pattern FindContactResponseMsg cID res = ResponseMsg FindContact (cID,res)
pattern FindValueResponseMsg   vID res = ResponseMsg FindValue   (vID,res)


-- | A 'Message' whose 'CMD' type 'c' is unknown.
data SomeMessage = forall mt mr c. (Typeable (Out c),Show (Resp c),Show (In c)) => SomeMessage (Message mt mr c)

-- | Attempt to decode a ByteString into 'SomeMessage'
decodeSomeMessage :: ByteString -> Maybe SomeMessage
decodeSomeMessage bs = Just <$> (`runGet` bs) $ do
    rt <- getWord8
    case rt of
        0 -> decodeSomeRequest
        1 -> decodeSomeResponse
        _ -> error "decodeSomeMessage"
  where
    decodeSomeRequest  = getWord8 >>= \tag -> case tag of
        0 -> SomeMessage . RequestMsg Ping        <$> get
        1 -> SomeMessage . RequestMsg Store       <$> get
        2 -> SomeMessage . RequestMsg FindContact <$> get
        3 -> SomeMessage . RequestMsg FindValue   <$> get
        _ -> error "decodeSomeRequest"

    decodeSomeResponse = getWord8 >>= \tag -> case tag of
        0 -> SomeMessage . ResponseMsg Ping        <$> get
        1 -> SomeMessage . ResponseMsg Store       <$> get
        2 -> SomeMessage . ResponseMsg FindContact <$> get
        3 -> SomeMessage . ResponseMsg FindValue   <$> get
        _ -> error "decodeSomeResponse"

