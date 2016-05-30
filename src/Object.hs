
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

newObject :: a -> STM (Object a)
newObject x = do
  n <- readTVar counter
  v <- newTVar x
  writeTVar counter $ n + 1
  pure $ Object { objectNumber = n, objectVar = v }

newObjectIO :: a -> IO (Object a)
newObjectIO = atomically . newObject

readObject :: Object a -> STM a
readObject = readTVar . objectVar

readObjectIO :: Object a -> IO a
readObjectIO = readTVarIO . objectVar

writeObject :: Object a -> a -> STM ()
writeObject obj = writeTVar $ objectVar obj

modifyObject :: Object a -> (a -> a) -> STM ()
modifyObject obj = modifyTVar $ objectVar obj

modifyObject' :: Object a -> (a -> a) -> STM ()
modifyObject' obj = modifyTVar' $ objectVar obj

swapObject :: Object a -> a -> STM a
swapObject obj = swapTVar $ objectVar obj

useObject :: Getting a s a -> Object s -> STM a
useObject q obj = view q <$> readObject obj

overObject :: ASetter s s a b -> (a -> b) -> Object s -> STM ()
overObject q f obj = modifyObject obj $ over q f

overObject' :: ASetter s s a b -> (a -> b) -> Object s -> STM ()
overObject' q f obj = modifyObject' obj $ over q f

