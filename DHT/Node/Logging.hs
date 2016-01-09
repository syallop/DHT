{-|
Stability : experimental

Defines a simple logging system which outputs all logged strings to stdout immediately.
 -}
module DHT.Node.Logging
  ( newLogging
  ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

import DHT

-- | Create a new logging system which outputs to stdout
newLogging :: IO (Logging IO)
newLogging = do
  loggingState <- newChan
  forkIO $ forever $ readChan loggingState >>= putStrLn
  return $ Just (writeChan loggingState)

