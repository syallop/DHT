{-|
Stability : experimental

Defines a simple logging system which outputs all logged strings to stdout immediately.
 -}
module DHT.SimpleNode.Logging
  ( newSimpleLogging
  ) where

import Control.Concurrent
import Control.Monad

import DHT.Client (Logging)

-- | Create a new logging system which outputs to stdout
newSimpleLogging :: IO (Logging IO)
newSimpleLogging = do
  loggingState <- newChan
  _threadID <- forkIO $ forever $ readChan loggingState >>= putStrLn
  return $ Just (writeChan loggingState)

