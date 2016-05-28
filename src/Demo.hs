
module Demo where

import Control.Concurrent.STM
import qualified Data.Set as S
import H.IO
import H.Prelude

import qualified CrossMap as CM
import Object
import Types

demo :: IO Game
demo = atomically $ do
  oHare <- newObject $ AirportState
    { _apCode     = "ORD"
    , _apName     = "Chicago O'Hare International Airport"
    , _apCapacity = 2400
    , _apAircraft = S.empty
    }
  midway <- newObject $ AirportState
    { _apCode     = "MDW"
    , _apName     = "Chicago Midway International Airport"
    , _apCapacity = 700
    , _apAircraft = S.empty
    }
  chicago <- newObject $ CityState
    { _cName       = "Chicago"
    , _cPopulation = 9700000
    , _cAirports   = S.fromList [oHare, midway]
    }
  msp <- newObject $ AirportState
    { _apCode     = "MSP"
    , _apName     = "Minneapolis–Saint Paul International Airport"
    , _apCapacity = 1100
    , _apAircraft = S.empty
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
    , _gMoney     = 100000
    }

