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
Create a default configuration by passing your address, a hash size and a
possible logging function to `mkSimpleNodeConfig`. Then pass this config, a
possible bootstrap node and a possible logging function to `newSimpleNode` to
execute.

```haskell
mkSimpleNodeConfig
  :: Addr                  -- ^ Our own address
  -> Int                   -- ^ Hash size
  -> LoggingOp IO          -- ^ An optional logging function
  -> IO (DHTConfig DHT IO)

newSimpleNode
  :: DHTConfig DHT IO       -- ^ Configuration, probably from 'mkSimpleNodeConfig'
  -> Maybe Addr             -- ^ Possible bootstrap address
  -> DHT IO a               -- ^ The DHT program to run
  -> IO (Either DHTError a)
```

## Example
For example, two nodes interact without logging.
```haskell
-- A ‘bootstrap’ node which does nothing itself at network address 192.168.0.1
do config <- mkSimpleNodeConfig (Addr "192.168.0.1" 6470) 32 Nothing
   newSimpleNode config Nothing $ forever threadDelay 1000000
```
```haskell
-- A node executing on 192.168.0.2 which stores a value (possibly at 192.168.0.1)
-- and instantly retrieves it. Hopefully.
do config <- mkSimpleNodeConfig (Addr "192.168.0.2" 6470) 32 Nothing
   newSimpleNode config (Just $ Addr "192.168.0.1" 6470) $ do
       id       <- store "Hello" " World!"
       (_,mStr) <- findValue id
       putStrLn $ case mStr of
           Just “ World!” -> “Success!”
           Just _         -> “We’re being lied to or... hash collision?”
           Nothing        -> “We can’t find it. Blame the hardware/ network!”
```

