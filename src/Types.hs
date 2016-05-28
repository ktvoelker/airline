
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens.TH
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import H.Prelude

import qualified CrossMap as CM

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
  { _acId    :: T.Text
  , _acModel :: Model
  } deriving (Eq, Ord, Show)

makeLenses ''Aircraft

data Airport =
  Airport
  { _apCode     :: T.Text
  , _apName     :: T.Text
  , _apCapacity :: Integer
  , _apAircraft :: S.Set Aircraft
  } deriving (Eq, Ord, Show)

makeLenses ''Airport

data City =
  City
  { _cName       :: T.Text
  , _cPopulation :: Integer
  , _cAirports   :: S.Set Airport
  } deriving (Eq, Ord, Show)

makeLenses ''City

data Game =
  Game
  { _gCities    :: V.Vector City
  , _gDistances :: CM.CrossMap Int Integer
  , _gAirborne  :: S.Set Aircraft
  , _gMoney     :: Integer
  } deriving (Eq, Ord, Show)

makeLenses ''Game

