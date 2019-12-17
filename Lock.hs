module Lock
  ( Lock(Lock)
  , newLock
  , withLock
  ) where

import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket_)

newtype Lock = Lock (MVar ())

newLock :: IO Lock
newLock = Lock <$> newMVar ()

acquire :: Lock -> IO ()
acquire (Lock m) = takeMVar m

release :: Lock -> IO ()
release (Lock m) = putMVar m ()

withLock :: Lock -> IO a -> IO a
withLock l = bracket_ (acquire l) (release l)
