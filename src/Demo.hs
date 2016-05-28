
module Demo where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S
import H.IO
import H.Prelude

import qualified CrossMap as CM
import Object
import Types

model737800 :: Model
model737800 =
  Model
  { _mCode  = "B738"
  , _mName  = "Boeing 737-800"
  , _mSpeed = 511
  , _mRange = 6340
  , _mSeats = 220
  , _mCost  = 10000
  }

demo :: IO Game
demo = atomically $ do
  ordMovements <- newTQueue
  oHare <- newObject $ AirportState
    { _apCode          = "ORD"
    , _apName          = "Chicago O'Hare International Airport"
    , _apCapacity      = 2400
    , _apAircraft      = S.empty
    , _apPending       = ordMovements
    , _apMovementDelay = 0
    }
  mdwMovements <- newTQueue
  midway <- newObject $ AirportState
    { _apCode          = "MDW"
    , _apName          = "Chicago Midway International Airport"
    , _apCapacity      = 700
    , _apAircraft      = S.empty
    , _apPending       = mdwMovements
    , _apMovementDelay = 0
    }
  chicago <- newObject $ CityState
    { _cName       = "Chicago"
    , _cPopulation = 9700000
    , _cAirports   = S.fromList [oHare, midway]
    }
  mspMovements <- newTQueue
  msp <- newObject $ AirportState
    { _apCode          = "MSP"
    , _apName          = "Minneapolis–Saint Paul International Airport"
    , _apCapacity      = 1100
    , _apAircraft      = S.empty
    , _apPending       = mspMovements
    , _apMovementDelay = 0
    }
  minneapolis <- newObject $ CityState
    { _cName       = "Minneapolis–St. Paul"
    , _cPopulation = 3500000
    , _cAirports   = S.fromList [msp]
    }
  newObject $ GameState
    { _gCities    = S.fromList [chicago, minneapolis]
    , _gDistances = CM.fromList [((oHare, midway), 2), ((oHare, msp), 354), ((midway, msp), 355)]
    , _gAirports  = S.fromList [oHare, midway, msp]
    , _gAircraft  = S.empty
    , _gAirborne  = S.empty
    , _gMoney     = 1000000
    , _gModels    = S.fromList [model737800]
    , _gFlights   = M.empty
    , _gSchedule  = M.empty
    , _gWeek      = 0
    , _gTime      = TimeOfWeek 0
    }

