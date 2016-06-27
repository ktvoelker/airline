
module Game where

import Control.Lens
import qualified Data.Map as M
import H.IO
import H.Prelude

import Object
import Parallel
import Types

gameSim :: Game -> IO ()
gameSim g = parallelize gameSplitter (gameProcessor g) gameJoiner g

gameSplitter :: Game -> IO [Airport]
gameSplitter g = view (gAirports . to M.elems) <$> readObject g

gameJoiner :: Game -> [()] -> IO ()
gameJoiner = const . const $ pure ()

gameProcessor :: Game -> Airport -> IO ()
gameProcessor = todo

