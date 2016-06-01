
module CLI.Output where

import qualified Data.Text as T
import H.Prelude

import Command.BuyAircraft
import Command.ShowAllAircraft
import Command.ShowAllAirports
import Types

class CLIResponse a where
  formatResponse :: a -> Text

instance CLIResponse () where
  formatResponse _ = "Done.\n"

instance CLIResponse BuyAircraftResponse where
  formatResponse (PurchasedAircraft code) = "Purchased aircraft: " <> unAircraftCode code <> ".\n"

instance CLIResponse AircraftList where
  formatResponse (AircraftList xs) = "Code   Model  Location\n" <> mconcat (map formatAircraft xs)

instance CLIResponse AirportList where
  formatResponse (AirportList xs) = "Code  Capacity  Present  Pending  Name\n" <> mconcat (map formatAirport xs)

formatField :: Bool -> Int -> Text -> Text
formatField rightJustified fieldLength xs = (if rightJustified then (affix <>) else (<> affix)) xs
  where
    remainingLen = fieldLength - T.length xs
    affix = T.replicate remainingLen " "

formatIntegral :: (Integral a, Show a) => Int -> a -> Text
formatIntegral fieldLength = formatField True fieldLength . show

formatAirport :: AirportResponse -> Text
formatAirport (code, capacity, present, pending, name) =
  formatField False 3 (unAirportCode code)
  <> "   " <> formatIntegral 8 capacity
  <> "  " <> formatIntegral 7 present
  <> "  " <> formatIntegral 7 pending
  <> "  " <> name
  <> "\n"

formatAircraft :: AircraftResponse -> Text
formatAircraft (aircraft, model, location) =
  formatField False 5 (unAircraftCode aircraft) 
  <> "  " <> formatField False 5 (unModelCode model)
  <> "  " <> formatField False 3 (formatLocation location)
  <> "\n"

formatLocation :: Either FlightResponse AirportCode -> Text
formatLocation = either formatAircraftFlight unAirportCode

formatAircraftFlight :: FlightResponse -> Text
formatAircraftFlight (origin, destination, traveled, distance) =
  unAirportCode origin <> "--"
  <> show (round $ miles traveled :: Integer) <> "/" <> show (round $ miles distance :: Integer)
  <> "->" <> unAirportCode destination

