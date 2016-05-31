
module Command
  ( module Command
  , module Command.Monad
  ) where

import Command.Monad

class Command a where
  type Response a :: *
  type Error a :: *
  runCommand :: a -> CM (Error a) (Response a)

