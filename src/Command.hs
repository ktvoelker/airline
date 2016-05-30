
module Command where

import Command.Monad

class Command a where
  type Response a :: *
  runCommand :: a -> CM (Response a)

