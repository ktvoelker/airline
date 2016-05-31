
module Demo where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S
import H.IO
import H.Prelude

import qualified CrossMap as CM
import Object
import Types
import Types.Time

b738Code :: ModelCode
b738Code = "B738"

b738 :: Model
b738 =
  Model
  { _mCode  = b738Code
  , _mName  = "Boeing 737-800"
  , _mSpeed = 511
  , _mRange = 6340
  , _mSeats = 220
  , _mCost  = 10000
  }

ordCode :: AirportCode
ordCode = "ORD"

mdwCode :: AirportCode
mdwCode = "MDW"

mspCode :: AirportCode
mspCode = "MSP"

demo :: IO Game
demo = atomically $ do
  ordMovements <- newTQueue
  oHare <- newObject $ AirportState
    { _apCode          = ordCode
    , _apName          = "Chicago O'Hare International Airport"
    , _apCapacity      = 2400
    , _apAircraft      = S.empty
    , _apPending       = ordMovements
    , _apPendingCount  = 0
    , _apMovementDelay = 0
    }
  mdwMovements <- newTQueue
  midway <- newObject $ AirportState
    { _apCode          = mdwCode
    , _apName          = "Chicago Midway International Airport"
    , _apCapacity      = 700
    , _apAircraft      = S.empty
    , _apPending       = mdwMovements
    , _apPendingCount  = 0
    , _apMovementDelay = 0
    }
  chicago <- newObject $ CityState
    { _cName       = "Chicago"
    , _cPopulation = 9700000
    , _cAirports   = S.fromList [oHare, midway]
    }
  mspMovements <- newTQueue
  msp <- newObject $ AirportState
    { _apCode          = mspCode
    , _apName          = "Minneapolis–Saint Paul International Airport"
    , _apCapacity      = 1100
    , _apAircraft      = S.empty
    , _apPending       = mspMovements
    , _apPendingCount  = 0
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
    , _gAirports  = M.fromList [(ordCode, oHare), (mdwCode, midway), (mspCode, msp)]
    , _gAircraft  = M.empty
    , _gAirborne  = S.empty
    , _gMoney     = 1000000
    , _gModels    = M.fromList [(b738Code, b738)]
    , _gFlights   = M.empty
    , _gSchedule  = M.empty
    , _gTime      = AbsoluteTime 0
    }

