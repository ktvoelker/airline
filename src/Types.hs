
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens.TH
import qualified Data.Set as S
import qualified Data.Text as T
import H.Prelude

import qualified CrossMap as CM
import Id

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

data Aircraft =
  Aircraft
  { _acModel :: Id Model
  } deriving (Eq, Ord, Show)

makeLenses ''Aircraft

data City =
  City
  { _cName       :: T.Text
  , _cPopulation :: Integer
  , _cAirports   :: IdSet Airport
  } deriving (Eq, Ord, Show)

data Airport =
  Airport
  { _apCode     :: T.Text
  , _apName     :: T.Text
  , _apCity     :: Id City
  , _apRunways  :: Integer
  , _apGates    :: Integer
  , _apAircraft :: S.Set (Id Aircraft)
  } deriving (Eq, Ord, Show)

makeLenses ''City

makeLenses ''Airport

data Game =
  Game
  { _gCities    :: IdMap City
  , _gDistances :: CM.CrossMap (Id City) Integer
  , _gAirports  :: IdMap Airport
  , _gModels    :: IdMap Model
  , _gAirborne  :: S.Set (Id Aircraft)
  , _gAircraft  :: IdMap Aircraft
  , _gMoney     :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''Game

