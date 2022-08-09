module Util (
  after,
  io,
  setNoBuffering
) where

import Control.Concurrent
import Control.Monad.IO.Class
import System.IO
after :: MonadIO m => Int -> m a -> m a
after t m = do io (threadDelay (t * 1000)); m

io :: MonadIO m => IO a -> m a
io = liftIO

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
