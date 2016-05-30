
module Command.RemoveFlight where

import H.Prelude

import Command
import Types

data RemoveFlight = RemoveFlight FlightNumber
  deriving (Show)

instance Command RemoveFlight where
  type Response RemoveFlight = ()
  runCommand _ = todo

