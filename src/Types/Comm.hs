
{-# LANGUAGE TemplateHaskell #-}
module Types.Comm where

import Data.Lens.Template
import qualified Data.Map as M
import H.Common

import Id
import Types

type RChan a = IO a

type WChan a = a -> IO ()

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
  { _chDepartures :: RChan Departure
  , _chArrivals   :: M.Map (Id Airport) (WChan Arrival)
  }

data AirportChannels =
  AirportChannels
  { _apchDepartures :: WChan Departure
  , _apchArrivals   :: RChan Arrival
  }

makeLenses [''Arrival, ''Departure, ''MainChannels, ''AirportChannels]

