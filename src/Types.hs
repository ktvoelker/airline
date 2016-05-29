
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Concurrent.STM
import Control.Lens.TH
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (IsString())
import qualified Data.Text as T
import H.Prelude

import qualified CrossMap as CM
import Object

newtype ModelCode = ModelCode { unModelCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data Model =
  Model
  { _mCode  :: ModelCode
  , _mName  :: T.Text
  , _mSpeed :: Integer
  , _mRange :: Integer
  , _mCost  :: Integer
  , _mSeats :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''Model

newtype AircraftCode = AircraftCode { unAircraftCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data AircraftState =
  AircraftState
  { _acCode  :: AircraftCode
  , _acModel :: Model
  } deriving (Show)

makeLenses ''AircraftState

newtype Minutes = Minutes Int
  deriving (Eq, Ord, Num, Real, Integral, Enum, Bounded, Show)

type Aircraft = Object AircraftState

data Movement = Landing AircraftFlight | Takeoff AircraftFlight
  deriving (Eq, Ord)

newtype AirportCode = AirportCode { unAirportCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data AirportState =
  AirportState
  { _apCode          :: AirportCode
  , _apName          :: T.Text
  , _apCapacity      :: Integer
  , _apAircraft      :: S.Set Aircraft
  , _apPending       :: TQueue Movement
  , _apPendingCount  :: Int
  , _apMovementDelay :: Minutes
  }

type Airport = Object AirportState

newtype TimeOfWeek = TimeOfWeek { offset :: Minutes }
  deriving (Eq, Ord, Show)

newtype FlightNumber = FlightNumber Int
  deriving (Eq, Ord, Show)

data Flight =
  Flight
  { _fNumber      :: Int
  , _fOrigin      :: Airport
  , _fDestination :: Airport
  , _fModel       :: Model
  , _fTimes       :: [TimeOfWeek]
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
  , _gAirports  :: M.Map AirportCode Airport
  , _gDistances :: CM.CrossMap Airport Integer
  , _gAircraft  :: M.Map AircraftCode Aircraft
  , _gAirborne  :: S.Set AircraftFlight
  , _gMoney     :: Integer
  , _gModels    :: M.Map ModelCode Model
  , _gFlights   :: M.Map FlightNumber Flight
  , _gSchedule  :: M.Map TimeOfWeek (S.Set Flight)
  , _gWeek      :: Integer
  , _gTime      :: TimeOfWeek
  }

makeLenses ''GameState

type Game = Object GameState

