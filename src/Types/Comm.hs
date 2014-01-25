
{-# LANGUAGE TemplateHaskell #-}
module Types.Comm where

import Data.Lens.Template
import qualified Data.Map as M
import H.Chan
import H.Common

import Id
import Types

data Arrival =
  Arrival
  { _arrAircraft :: Id Aircraft
  , _arrOrigin   :: Id Airport
  } deriving (Eq, Ord, Show)

data Departure =
  Departure
  { _depAircraft    :: Id Aircraft
  , _depOrigin      :: Id Airport
  , _depDestination :: Id Airport
  } deriving (Eq, Ord, Show)

data MainChannels =
  MainChannels
  { _chDepartures :: ReadChan Departure
  , _chArrivals   :: M.Map (Id Airport) (WriteChan Arrival)
  }

data AirportChannels =
  AirportChannels
  { _apchDepartures :: WriteChan Departure
  , _apchArrivals   :: ReadChan Arrival
  }

makeLenses [''Arrival, ''Departure, ''MainChannels, ''AirportChannels]

