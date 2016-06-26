module Parallel where

import Control.Concurrent
import Data.List (replicate, splitAt)
import H.IO
import H.Prelude

parallelize :: (a -> IO [b]) -> (b -> IO c) -> (a -> [c] -> IO d) -> a -> IO d
parallelize splitter processor joiner x = do
  n <- getNumCapabilities
  finishedParts <- newChan
  (firstParts, laterParts) <- splitAt n <$> splitter x
  let process p = forkIO $ processor p >>= writeChan finishedParts
  mapM_ process firstParts
  initialFinishedParts <- forM laterParts $ \p -> do
    f <- readChan finishedParts
    void $ process p
    pure f
  lastFinishedParts <-
    forM (replicate (length firstParts) ()) . const $ readChan finishedParts
  joiner x $ initialFinishedParts <> lastFinishedParts

