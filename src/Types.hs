
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
import Types.Time

newtype Distance = Distance { miles :: Double }
  deriving (Eq, Ord, Show)

newtype Speed = Speed { milesPerMinute :: Double }
  deriving (Eq, Ord, Show)

distanceToTimeAtSpeed :: Distance -> Speed -> Minutes
distanceToTimeAtSpeed Distance{..} Speed{..} = Minutes . round $ miles / milesPerMinute

newtype Money = Money { dollars :: Integer }
  deriving (Eq, Ord, Show)

plusMoney :: Money -> Money -> Money
plusMoney a b = Money $ dollars a + dollars b

minusMoney :: Money -> Money -> Money
minusMoney a b = Money $ dollars a - dollars b

newtype ModelCode = ModelCode { unModelCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data Model =
  Model
  { _mCode  :: ModelCode
  , _mName  :: T.Text
  , _mSpeed :: Speed
  , _mRange :: Distance
  , _mCost  :: Money
  , _mSeats :: Int
  } deriving (Eq, Ord, Show)

makeLenses ''Model

newtype AircraftCode = AircraftCode { unAircraftCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data AircraftState =
  AircraftState
  { _acCode     :: AircraftCode
  , _acModel    :: Model
  , _acLocation :: Maybe Airport
  }

type Aircraft = Object AircraftState

data Movement = Landing AircraftFlight | Takeoff AircraftFlight
  deriving (Eq, Ord)

newtype AirportCode = AirportCode { unAirportCode :: T.Text }
  deriving (Eq, Ord, IsString, Show)

data AirportState =
  AirportState
  { _apCode          :: AirportCode
  , _apName          :: T.Text
  , _apCapacity      :: Int
  , _apAircraft      :: S.Set Aircraft
  , _apPending       :: TQueue Movement
  , _apPendingCount  :: Int
  , _apMovementDelay :: Minutes
  }

type Airport = Object AirportState

newtype FlightNumber = FlightNumber Int
  deriving (Eq, Ord, Show)

data Flight =
  Flight
  { _fNumber      :: FlightNumber
  , _fOrigin      :: Airport
  , _fDestination :: Airport
  , _fModel       :: Model
  , _fDays        :: S.Set DayOfWeek
  , _fTime        :: TimeOfDay
  } deriving (Eq, Ord)

data AircraftFlightState =
  AircraftFlightState
  { _afsAircraft :: Aircraft
  , _afsFlight   :: Flight
  , _afsTraveled :: Distance
  } deriving (Eq, Ord)

type AircraftFlight = Object AircraftFlightState

makeLenses ''AircraftState

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
  , _gDistances :: CM.CrossMap Airport Distance
  , _gAircraft  :: M.Map AircraftCode Aircraft
  , _gAirborne  :: S.Set AircraftFlight
  , _gMoney     :: Money
  , _gModels    :: M.Map ModelCode Model
  , _gFlights   :: M.Map FlightNumber Flight
  , _gSchedule  :: M.Map TimeOfWeek (S.Set Flight)
  , _gTime      :: AbsoluteTime
  }

makeLenses ''GameState

type Game = Object GameState

