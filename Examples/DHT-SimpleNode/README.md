## Simple DHT Node
This example is a concrete implementation of 'DHT' using UDP, MVars, in-memory hashmaps etc. This example has its own example (yo dawg...) under
'Examples/DHT-SimpleNode-Example' which runs several
SimpleNodes and tests their operations.

## Module Structure

| Module                      | Description                                                                                                                         |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| DHT.SimpleNode              | A simple concrete DHT implementation using the choices under `DHT.SimpleNode.*`. UDP, MVars, `DHT.Routing`, in-memory hashmaps, etc |
| DHT.SimpleNode.Logging      | Uses Control.Concurrent.Chan                                                                                                        |
| DHT.SimpleNode.Messaging    | Uses Network.Socket, MVars and UDP                                                                                                  |
| DHT.SimpleNode.RoutingTable | Wraps DHT.Routing in a MVar for concurrent access                                                                                   |
| DHT.SimpleNode.ValueStore   | Stores data in a Data.Map                                                                                                           |

## Usage
Call

```haskell
newSimpleNode
  :: Addr
  -> Maybe Addr
  -> LoggingOp IO
  -> DHT IO a
  -> IO (Either DHTError a)
```

with your Addr, a possible bootstrap Addr, possible logging operation and the DHT program to execute.

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

