
module View where

import Control.Concurrent
import Graphics.Vty.Widgets.All
import H.Common

data View = View { _vStopped :: MVar () }

startView :: (MonadIO m) => m View
startView = liftIO $ do
  query <- editWidget
  onActivate query $ const stopView
  fg <- newFocusGroup
  void $ addToFocusGroup fg query
  coll <- newCollection
  void $ addToCollection coll query fg
  stopped <- newEmptyMVar
  void $ forkIO $ do
    runUi coll defaultContext
    putMVar stopped ()
  return $ View stopped

waitForView :: (MonadIO m) => View -> m ()
waitForView = liftIO . takeMVar . _vStopped

stopView :: (MonadIO m) => m ()
stopView = liftIO $ schedule shutdownUi

