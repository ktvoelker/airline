
module Object
  ( Object()
  , newObject
  , newObjectIO
  , readObject
  , readObjectIO
  , writeObject
  , modifyObject
  , modifyObject'
  , swapObject
  , useObject
  , overObject
  , overObject'
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.STM.Class
import H.IO
import H.Prelude
import System.IO.Unsafe

data Object a = Object { objectNumber :: Integer, objectVar :: (TVar a) }
  deriving (Eq)

instance Ord (Object a) where
  compare (Object m _) (Object n _) = compare m n

{-# NOINLINE counter #-}
counter :: TVar Integer
counter = unsafePerformIO $ newTVarIO 0

newObject :: (MonadSTM m) => a -> m (Object a)
newObject x = liftSTM $ do
  n <- readTVar counter
  v <- newTVar x
  writeTVar counter $ n + 1
  pure $ Object { objectNumber = n, objectVar = v }

newObjectIO :: a -> IO (Object a)
newObjectIO = atomically . newObject

readObject :: (MonadSTM m) => Object a -> m a
readObject = liftSTM . readTVar . objectVar

readObjectIO :: Object a -> IO a
readObjectIO = readTVarIO . objectVar

writeObject :: (MonadSTM m) => Object a -> a -> m ()
writeObject obj = liftSTM . writeTVar (objectVar obj)

modifyObject :: (MonadSTM m) => Object a -> (a -> a) -> m ()
modifyObject obj = liftSTM . modifyTVar (objectVar obj)

modifyObject' :: (MonadSTM m) => Object a -> (a -> a) -> m ()
modifyObject' obj = liftSTM . modifyTVar' (objectVar obj)

swapObject :: (MonadSTM m) => Object a -> a -> m a
swapObject obj = liftSTM . swapTVar (objectVar obj)

useObject :: (MonadSTM m) => Getting a s a -> Object s -> m a
useObject q obj = liftSTM $ view q <$> readObject obj

overObject :: (MonadSTM m) => ASetter s s a b -> (a -> b) -> Object s -> m ()
overObject q f obj = liftSTM $ modifyObject obj $ over q f

overObject' :: (MonadSTM m) => ASetter s s a b -> (a -> b) -> Object s -> m ()
overObject' q f obj = liftSTM $ modifyObject' obj $ over q f

