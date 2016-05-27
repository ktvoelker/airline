
module Types.Command where

import H.Prelude

data Command =
    Pass
  | Quit
  deriving (Eq, Show)

