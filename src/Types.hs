
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Concurrent.STM
import Control.Lens.TH
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import H.Prelude

import qualified CrossMap as CM
import Object

data Model =
  Model
  { _mName  :: T.Text
  , _mSpeed :: Integer
  , _mRange :: Integer
  , _mCost  :: Integer
  , _mSeats :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''Model

data AircraftState =
  AircraftState
  { _acId    :: T.Text
  , _acModel :: Model
  } deriving (Show)

makeLenses ''AircraftState

newtype Minutes = Minutes Int
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded, Show)

type Aircraft = Object AircraftState

data Movement = Landing AircraftFlight | Takeoff AircraftFlight
  deriving (Eq, Ord)

data AirportState =
  AirportState
  { _apCode          :: T.Text
  , _apName          :: T.Text
  , _apCapacity      :: Integer
  , _apAircraft      :: S.Set Aircraft
  , _apPending       :: TQueue Movement
  , _apMovementDelay :: Minutes
  }

type Airport = Object AirportState

newtype TimeOfWeek = TimeOfWeek { offset :: Minutes }
  deriving (Eq, Ord, Show)

data Flight =
  Flight
  { _fOrigin      :: Airport
  , _fDestination :: Airport
  , _fModel       :: Model
  , _fTime        :: TimeOfWeek
  } deriving (Eq, Ord)

data AircraftFlightState =
  AircraftFlightState
  { _afsAircraft :: Aircraft
  , _afsFlight   :: Flight
  } deriving (Eq, Ord)

type AircraftFlight = Object AircraftFlightState

makeLenses ''AirportState

makeLenses ''Flight

makeLenses ''AircraftFlightState

data CityState =
  CityState
  { _cName       :: T.Text
  , _cPopulation :: Integer
  , _cAirports   :: S.Set Airport
  }

makeLenses ''CityState

type City = Object CityState

data GameState =
  GameState
  { _gCities    :: S.Set City
  , _gAirports  :: S.Set Airport
  , _gDistances :: CM.CrossMap Airport Integer
  , _gAircraft  :: S.Set Aircraft
  , _gAirborne  :: S.Set AircraftFlight
  , _gMoney     :: Integer
  , _gModels    :: S.Set Model
  , _gFlights   :: M.Map TimeOfWeek (S.Set Flight)
  , _gWeek      :: Integer
  , _gTime      :: TimeOfWeek
  }

makeLenses ''GameState

type Game = Object GameState

