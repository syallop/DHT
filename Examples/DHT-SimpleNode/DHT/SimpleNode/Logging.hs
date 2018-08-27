{-|
Stability : experimental

Defines a simple logging system which outputs all logged strings to stdout immediately.
 -}
module DHT.SimpleNode.Logging
  ( newSimpleLogging
  ) where

import Control.Concurrent
import Control.Monad

import DHT

-- | Create a new logging system which outputs to stdout
newSimpleLogging :: IO (LoggingOp IO)
newSimpleLogging = do
  loggingState <- newChan
  forkIO $ forever $ readChan loggingState >>= putStrLn
  return $ Just (writeChan loggingState)

