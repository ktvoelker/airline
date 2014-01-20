
module GlobalState where

-- import qualified Data.Map as M
-- import qualified Data.Set as S
import qualified Data.Text as T

import qualified CrossMap as CM
import Id

data City =
  City
  { cityName       :: T.Text
  , cityPopulation :: Integer
  , cityAirports   :: IdSet Airport
  } deriving (Eq, Ord, Show)

data Airport =
  Airport
  { airportCode :: T.Text
  , airportName :: T.Text
  , airportCity :: Id City
  } deriving (Eq, Ord, Show)

data Aircraft =
  Aircraft
  { aircraftName  :: T.Text
  , aircraftSpeed :: Integer
  , aircraftRange :: Integer
  , aircraftCost  :: Integer
  , aircraftFuel  :: Integer
  , aircraftSeats :: Integer
  } deriving (Eq, Ord, Show)

data GlobalState =
  GlobalState
  { cities    :: IdMap City
  , distances :: CM.CrossMap CityId Integer
  , airports  :: IdMap Airport
  , aircraft  :: IdMap Aircraft
  }

