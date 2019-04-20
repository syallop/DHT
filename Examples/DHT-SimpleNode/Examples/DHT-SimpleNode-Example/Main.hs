{-# LANGUAGE OverloadedStrings #-}
module Main where

import DHT.SimpleNode
import DHT.SimpleNode.Logging

import DHT
import DHT.Address
import DHT.Bucket
import DHT.ID
import DHT.Contact


import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.ByteString.Lazy
import Data.String.Conv
import qualified Data.ByteString.Lazy.Char8 as Lazy

-- delay a thread for n seconds
delay :: Int -> IO ()
delay = threadDelay . (* 1000000)

forkVoid :: IO a -> IO ThreadId
forkVoid m = forkIO $ void m

forkVoid_ :: IO a -> IO ()
forkVoid_ m = void (forkVoid m)

bootstrapAddr :: Address
bootstrapAddr = fromParts (IPV4 "127.0.0.1") [UDP 6471]

idle :: DHT IO a
idle = liftDHT $ forever $ threadDelay 5000000

-- Add a prefix to a logged string
lgPrefix :: String -> String -> DHT IO ()
lgPrefix p str = lg (p <> str)

-- Prefix a log string with a name and our id
lgAt :: String -> String -> DHT IO ()
lgAt name str = do
  id <- askOurID
  lgPrefix (show id <> " " <> name <> " :\t") str

-- Store two values in the DHT, then show their IDs then try and retrieve the
-- values, expecting them to be the same.
testStore :: DHT IO ()
testStore = do
  let key0 = "Hello"
      val0 = "World!"

      key1 = "foo"
      val1 = "barbaz"

  lgHere . toS $ "Storing two values: " <> key0<>":"<>val0<>" and " <> key1<>":"<>val1
  keyID0  <- store key0 val0
  keyID1  <- store key1 val1
  lgHere $ "The IDs of the two keys are: " ++ show keyID0 ++ " and " ++ show keyID1

  lgHere "Looking up the two ID's to check they exist/ have the right value."
  mVal0 <- findValue keyID0
  mVal1 <- findValue keyID1
  responseValueIs mVal0 val0
  responseValueIs mVal1 val1

  return ()
  where
    lgHere = lgAt "testStore"

    -- Require that a response from 'findValue' is the same as a given value.
    -- Output related information.
    responseValueIs :: ([Contact],Maybe ByteString) -> ByteString -> DHT IO ()
    responseValueIs (cts,mVal) expectedVal =
      lgHere $ "Told about contacts: " ++ showContacts cts ++ (case mVal of
        Nothing
          -> " But did NOT get any value back. FAILURE."

        Just v
          | v == expectedVal
          -> " and got expected value back. SUCCESS."
          | otherwise
          -> " But got a different value back. FAILURE.")

-- Lookup two ID's communicated out of band in the DHT.
-- We hope to retrieve their values.
testLookup :: DHT IO ()
testLookup = do
  lgHere "Assuming values might have been stored by somebody else for \"Hello\" and \"foo\", lookup the values."
  hashSize <- askHashSize
  mVal0 <- findValue (mkID ("Hello"::Lazy.ByteString) hashSize)
  mVal1 <- findValue (mkID ("foo"::Lazy.ByteString) hashSize)
  lgFindResponse mVal0
  lgFindResponse mVal1
  return ()
  where
    lgHere = lgAt "testLookup"

    lgFindResponse :: ([Contact],Maybe ByteString) -> DHT IO ()
    lgFindResponse (ctcs,mVal) =
      lgHere $ "Told about contacts: " ++ showContacts ctcs ++ (
        case mVal of
          Nothing
            -> mconcat [" But we did NOT get a value back."
                       ," This is only a FAILURE IF 'testStore' has stored these values before us."
                       ," Otherwise we're simply looking up IDs which don't exist."
                       ]

          Just v
            -> mconcat [" and we got the value: " ++ toS v
                       ," back."
                       ]
      )

-- Attempt to find our own closest neighbours by performing a 'findContact' on
-- our own ID.
testNeighbours :: DHT IO ()
testNeighbours = do
  id <- askOurID
  lgHere $ "Our ID is" ++ show id

  lgHere "Attempt to find the neighbours of our ID"
  (ns,mn) <- findContact id
  lgHere $ (case mn of
    Nothing
      -> "We're not already known about"

    Just i
      | _ID i == id
      -> "We found ourself"
      | otherwise
      -> "We found a collision with ourself!!"
      ) ++ " and found neighbours: " ++ showContacts ns
  where
    lgHere = lgAt "testNeighbours"

-- Log our name, but otherwise do nothing.
doNothing :: String -> DHT IO ()
doNothing name = do
  lgAt name "Doing nothing"
  return ()

main :: IO ()
main = do
  -- Create a logger to share across our example nodes
  mLogging <- newSimpleLogging
  let hashSize = 8

      -- Make the config for one of our test nodes at a given addr
      -- Decide whether to bootstrap off the designated bootstrap address.
      mkConfig :: Address -> Bool -> IO (DHTConfig DHT IO)
      mkConfig ourAddr shouldBootstrap
        = let mBootstrapAddr = if shouldBootstrap then Just bootstrapAddr else Nothing
             in mkSimpleNodeConfig ourAddr hashSize mLogging mBootstrapAddr

      -- Asynchronously run a test node with a name and start delay,
      -- deciding whether to bootstrap off the designated bootstrap address.
      --
      -- DHTPrograms remain idle after execution to allow them to continue
      -- serving requests from other nodes.
      run :: String    -- Test name
          -> Int       -- Delay before executing
          -> Bool      -- Whether to bootstrap of the hardcoded bootstrapAddr
          -> Address   -- Nodes own address.
          -> DHT IO a  -- DHT computation to execute
          -> IO ()
      run testName startDelay shouldBootstrap ourAddr dhtProgram
        = forkVoid_ $ do config <- mkConfig ourAddr shouldBootstrap
                         newSimpleNode config $ do lg $ "Creating " ++ testName ++ " node."
                                                   liftDHT $ delay startDelay
                                                   dhtProgram
                                                   idle

  -- Create the first node others will use as a bootstrap.
  run "bootstrap" 0 False bootstrapAddr $ return ()

  -- Create several other nodes.
  forM [6472 .. 6480] $ \port -> do let name = ("bootstrap" <> show port)
                                    run name 0 True (fromParts (IPV4 "127.0.0.1") [UDP port]) $ doNothing name
                                    delay 1
  -- Test storing and retrieving a value
  run "testStore" 1 True (fromParts (IPV4 "127.0.0.1") [UDP 6481]) testStore

  -- Test looking up a value WE didnt store
  run "testLookup" 5 True (fromParts (IPV4 "127.0.0.1") [UDP 6482]) testLookup

  -- Test neighbour lookup
  run "testNeighbours" 8 True (fromParts (IPV4 "127.0.0.1") [UDP 6483]) testNeighbours

  delay 10
  return ()

