
module Command where

import H.Prelude

import Command.Monad
import Types (Game())

newtype CSTM e a = CSTM { unCSTM :: StateT Game (ExceptT e STM) a }
  deriving (Functor, Applicative, Monad, MonadState Game, MonadError e)

class Command a where
  type Response a :: *
  type Error a :: *
  runCommand :: a -> ExceptT (Error a) CM (Response a)

atomically' :: CSTM e a -> ExceptT e CM a
atomically' m = lift getGame >>= mapExceptT atomically . evalStateT (unCSTM m)

liftSTM :: STM a -> CSTM e a
liftSTM = CSTM . lift . lift

