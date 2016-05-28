
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens.TH
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
  , _mFuel  :: Integer
  , _mSeats :: Integer
  , _mPrice :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''Model

data AircraftState =
  AircraftState
  { _acId    :: T.Text
  , _acModel :: Model
  } deriving (Eq, Ord, Show)

makeLenses ''AircraftState

type Aircraft = Object AircraftState

data AirportState =
  AirportState
  { _apCode     :: T.Text
  , _apName     :: T.Text
  , _apCapacity :: Integer
  , _apAircraft :: S.Set Aircraft
  } deriving (Eq, Ord)

makeLenses ''AirportState

type Airport = Object AirportState

data CityState =
  CityState
  { _cName       :: T.Text
  , _cPopulation :: Integer
  , _cAirports   :: S.Set Airport
  } deriving (Eq, Ord)

makeLenses ''CityState

type City = Object CityState

data GameState =
  GameState
  { _gCities    :: S.Set City
  , _gAirports  :: S.Set Airport
  , _gDistances :: CM.CrossMap Airport Integer
  , _gAircraft  :: S.Set Aircraft
  , _gAirborne  :: S.Set Aircraft
  , _gMoney     :: Integer
  } deriving (Eq, Ord)

makeLenses ''GameState

type Game = Object GameState

