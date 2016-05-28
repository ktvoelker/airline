
module Demo where

import qualified Data.Map as M
import qualified Data.Set as S

import qualified CrossMap as CM
import Id
import Types

oHare :: Airport
oHare =
  Airport
  { _apCode     = "ORD"
  , _apName     = "Chicago O'Hare International Airport"
  , _apCapacity = 2400
  , _apAircraft = S.empty
  }

midway :: Airport
midway =
  Airport
  { _apCode     = "MDW"
  , _apName     = "Chicago Midway International Airport"
  , _apCapacity = 700
  , _apAircraft = S.empty
  }

chicago :: City
chicago =
  City
  { _cName       = "Chicago"
  , _cPopulation = 9700000
  , _cAirports   = S.fromList [oHare, midway]
  }

msp :: Airport
msp =
  Airport
  { _apCode     = "MSP"
  , _apName     = "Minneapolis–Saint Paul International Airport"
  , _apCapacity = 1100
  , _apAircraft = S.empty
  }

minneapolis :: City
minneapolis =
  City
  { _cName       = "Minneapolis–St. Paul"
  , _cPopulation = 3500000
  , _cAirports   = S.fromList [msp]
  }

chicagoId :: Id City
chicagoId = firstId

minneapolisId :: Id City
minneapolisId = nextId chicagoId

demo :: Game
demo =
  Game
  { _gCities    = M.fromList [(chicagoId, chicago), (minneapolisId, minneapolis)]
  , _gDistances = CM.fromList [((chicagoId, minneapolisId), 354)]
  , _gAirborne  = S.empty
  , _gMoney     = 100000
  }

