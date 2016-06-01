
module Command
  ( module Command
  , module Command.Monad
  , throw
  , throwIO
  ) where

import Control.Concurrent.STM
import Control.Exception
import H.Prelude

import Command.Monad
import Types

class Command a where
  type Response a :: *
  runCommand :: a -> CM (Response a)

data CommandError =
    InvalidAirport AirportCode
  | InvalidModel ModelCode
  | MissingParameter
  | NotEnoughMoney
  | RouteTooShort
  deriving (Show)

instance Exception CommandError where

