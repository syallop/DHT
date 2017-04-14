{-# LANGUAGE
    DataKinds
  , GADTs
  , KindSignatures
  , TypeFamilies
  , UndecidableInstances
  #-}
{-|
Stability : experimental

Model the commands a dht must be able to handle and issue.
'Commands' are singletons indexed by a unique (c::CMD) type.

Closed type functions determine for each cmd type:
- Target: Either targeted at individual Addrs or the DHT as a whole
- In: Input to a command.
- Out: Expected output.
- Resp: The type required to respond to a recieved command such that the sender can recognise it and extract the Out
 -}
module DHT.Command
    ( CMD(PING,STORE,FINDVALUE,FINDCONTACT)
    , Command(Ping,Store,FindValue,FindContact)

    , Target
    , In
    , Out
    , Resp

    , commandTag
    , compareCommand
    ) where

import Data.Function

import DHT.Contact
import DHT.ID

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Word8

-- | DataKind promoted tag of command variants
data CMD = PING | STORE | FINDVALUE | FINDCONTACT
  deriving (Ord,Eq)

-- | Commands handled by a dht.
-- Indexed by CMD as singletons.
-- I.E.
--  Ping :: Command PING
--  Foo  :: Command FOO
--  etc.
data Command (c :: CMD) where
  Ping        :: Command 'PING
  Store       :: Command 'STORE
  FindContact :: Command 'FINDCONTACT
  FindValue   :: Command 'FINDVALUE

-- Translate a Command to its corresponding CMD
cmd :: Command c -> CMD
cmd Ping = PING
cmd Store = STORE
cmd FindContact = FINDCONTACT
cmd FindValue = FINDVALUE

-- | Compare two 'Command's
compareCommand :: Command c -> Command c' -> Ordering
compareCommand command0 command1 = cmd command0 `compare` cmd command1

instance Ord (Command c) where compare = compareCommand
instance Eq (Command c) where (==) = on (==) cmd

instance Show (Command c) where
  show c = case c of
    Ping -> "Ping"
    Store -> "Store"
    FindContact -> "FindContact"
    FindValue -> "FindValue"

-- | The Target of a 'CMD' is either typed 'Addr', indicating it is sent to a specific Address
-- or it is typed 'Either () Addr' indicating it may be targeted at the DHT as a whole ( () ) as well
-- as at a specific Address.
type family Target (cmd :: CMD) where
    Target 'PING        = Addr
    Target 'STORE       = Addr
    Target 'FINDVALUE   = Either () Addr
    Target 'FINDCONTACT = Either () Addr

-- | A 'CMD' has an input when sent.
type family In (cmd :: CMD) where
    In 'PING        = Int
    In 'STORE       = ByteString
    In 'FINDVALUE   = ID
    In 'FINDCONTACT = ID

-- | A 'CMD@ has an expected output type when received.
type family Out (cmd :: CMD) where
    Out 'PING        = Int
    Out 'STORE       = ID
    Out 'FINDVALUE   = ([Contact],Maybe ByteString)
    Out 'FINDCONTACT = ([Contact],Maybe Contact)

-- | When returning a response to a received 'CMD', data is attatched which is either
-- just an 'Out'put value, or an Output value paired with the 'In'put being responded to.
type family Resp (cmd :: CMD) where
    Resp 'PING        = Out 'PING
    Resp 'STORE       = Out 'STORE
    Resp 'FINDVALUE   = (In 'FINDVALUE,Out 'FINDVALUE)
    Resp 'FINDCONTACT = (In 'FINDCONTACT,Out 'FINDCONTACT)

-- | Map Commands to a unique tag value
commandTag :: Command c -> Word8
commandTag command = case command of
    Ping        -> 0
    Store       -> 1
    FindContact -> 2
    FindValue   -> 3

