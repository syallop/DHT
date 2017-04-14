# Haskell Distributed HashTable
This project defines an API for building a distributed HashTable in Haskell,
built upon a small set of core commands and abstracted over several
dependencies such as messaging; routing; storage and logging.

A concrete example implementation is found under 'Examples/DHT-SimpleNode' which
instantiates a DHT using UDP, MVars, in-memory hashmaps etc. This example has its own example (yo dawg...) under
'Examples/DHT-SimpleNode/Examples/DHT-SimpleNode-Example' which runs several
SimpleNodes and tests their operations.

## Module Structure
Some of the main modules are:

| Module      | Description                                                                                                                                                                       |
| ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DHT         | An API for writing DHT programs and executing them with given configurations                                                                                                      |
| DHT.Command | Core commands (`Ping`,`Store`,`FindContact`,`FindValue`) tagged at the type level with the types required to send, accept and respond to them as well as what they’re targeted at |
| DHT.ID      | A fixed length string of bits identifying instances AND values in the same ID-space. ID’s have a notion of distance to each other by an XOR metric.                               |
| DHT.Contact | Associate contact ID’s to their `Addr`esses and other metadata                                                                                                                    |
| DHT.Routing | A Routing table which remembers `Contact`s by their distance to ourself, such that many close contacts and few far away contacts are kept. Allowing log(n) routing                |

In the DHT-SimpleNode example package:

| Module         | Description                                                                                                                         |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| DHT.SimpleNode | A simple concrete DHT implementation using the choices under `DHT.SimpleNode.*`. UDP, MVars, `DHT.Routing`, in-memory hashmaps, etc |

## Commands
The core exported DHT commands are:

| Command     | Type                                       | Meaning                                                                              |
| ----------- | ------------------------------------------ | ------------------------------------------------------------------------------------ |
| ping        | `Addr -> DHT m ()`                         | Test a DHT replies with the same ping’d int                                          |
| store       | `ByteString -> DHT m ID`                   | Store a `ByteString` value at the k-nearest places, returning an ID                  |
| findValue   | `ID -> DHT m ([Contact],Maybe ByteString)` | Retrieve a stored ByteString value with the given ID                                 |
| findContact | `ID -> DHT m ([Contact],Maybe Contact)`    | Attempt to find the contact with the given ID. Also produce the k-nearest neighbours |

Programs can be built by monadically chaining these commands E.G.
```haskell
{-# LANGUAGE OverloadedStrings #-}
dht :: DHT m ()
dht = do
    ping (Addr “127.0.0.1” 1234)
    helloID     <- store “Hello World!”
    (cs,mHello) <- findValue helloID
    ...
    return ()
```
Which attempts to ping an address (which may not have been seen before)
. then stores a value at places deemed appropriate and finally attempts
to retrieve it along with a list of contacts deemed ‘near’.

## Execution
Programs can be ran by calling a ‘runDHT’ function which is supplied a variety of configuration options
and into which several dependent subsystems are injected.
```haskell
runDHT
  :: (Monad m,Functor m)
  => Addr                    -- ^ Own address
  -> Int                     -- ^ Size of IDs
  -> m Time                  -- ^ Current time
  -> m Int                   -- ^ Random Int
  -> Messaging m             -- ^ Send Bytes, wait on sent commands and route received commands.
  -> RoutingTable m          -- ^ Insert and lookup of (some) known addresses
  -> ValueStore m            -- ^ Store and retrieve values by ID
  -> Logging m               -- ^ How and whether to log output
  -> Maybe Addr              -- ^ Possible bootstrap address
  -> DHT m a                 -- ^ Computation to run
  -> m (Either DHTError a)   -- ^ Error or result in some Monad ‘m’.
```

The larger subsystems are responsible for:

| Subsystem    | Responsibilities                                                                                                                                                                                            |
| ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Messaging    | <ul> <li>Establishing connections and physically transmitting data between contacts</li><li>Sending commands and waiting for a response</li><li>Routing responses to the correct waiting location</li></ul> |
| RoutingTable | <ul> <li>Storage and retrieval of encountered contacts </li> <li>Deciding WHICH contacts to keep and which to drop</li>                                                                                     |
| ValueStore   | <ul> <li>Storage and retrieval of stored ByteStrings associated to an ID</li></ul>                                                                                                                          |
| Logging      | <ul> <li>Whether and how to log given Strings</li> </ul>                                                                                                                                                    |

Some simple example implementations are found in the DHT-SimpleNode example, all of which use IO as the base ‘m’:

| Module                      | Exports                                                                 | Description                                                                                                                                                                                                                    |
| --------------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| DHT.SimpleNode.Logging      | `newSimpleLogging      :: IO (Logging IO)`                              | Logs to stdout in a queue using Chans.                                                                                                                                                                                         |
| DHT.SimpleNode.Messaging    | `newSimpleMessaging    :: IDSize -> (Length,Port) -> IO (Messaging IO)` | UDP sockets, socket reuse, wire format prepends reply port to messages and pads port numbers to a fixed length. Routes and waits messages with MVars and a notion of pattern matching (rather than E.G. sending unique tokens) |
| DHT.SimpleNode.RoutingTable | `newSimpleRoutingTable :: IDSize -> ID -> Time -> IO (RoutingTable IO)` | Wraps the provided DHT.Routing into a shared state MVar.                                                                                                                                                                       |
| DHT.SimpleNode.ValueStore   | `newSimpleValueStore   :: IO (ValueStore IO)`                           | A MVar shared in-memory hashmap of IDs to ByteStrings                                                                                                                                                                          |

“DHT.SimpleNode” then ties all these choices together into a concrete implementation:
```haskell
newSimpleNode
  :: Addr                  -- ^ Our own address
  -> Maybe Addr            -- ^ Possible bootstrap address
  -> Logging IO            -- ^ An optional logging function
  -> DHT IO a              -- ^ The DHT program to run
  -> IO (Either DHTError a)
```

For example, two nodes interact without logging.
```haskell
-- A ‘bootstrap’ node which does nothing itself at network address 192.168.0.1
newSimpleNode (Addr “192.168.0.1” 6470) Nothing Nothing $ forever $ threadDelay 1000000
```
```haskell
-- A node executing on 192.168.0.2 which stores a value (possibly at 192.168.0.1)
-- and instantly retrieves it. Hopefully.
newSimpleNode (Addr “192.168.0.2” 6470) (Just $ Addr “192.168.0.1) Nothing $ do
    id       <- store “Hello World!”
    (_,mStr) <- findValue id
    putStrLn $ case mStr of
        Just “Hello World!” -> “Success!”
        Just _              -> “We’re being lied to or... hash collision?”
        Nothing             -> “We can’t find it. Blame the hardware/ network!”
```

