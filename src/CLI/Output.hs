
module CLI.Output where

import qualified Data.Text as T
import H.Prelude

import Types
import Types.Command

formatResponse :: Response -> Text
formatResponse = \case
  NoResponse -> "Done.\n"
  ErrorResponse xs -> xs <> "\n"
  AirportList xs -> "Code  Capacity  Present  Pending  Name\n" <> mconcat (map formatAirport xs)
  AircraftList xs -> "Code   Model  Location\n" <> mconcat (map formatAircraft xs)
  PurchasedAircraft code -> "Purchased aircraft: " <> unAircraftCode code <> ".\n"
  NotEnoughMoney -> "Not enough money.\n"
  InvalidAirport code -> "Invalid airport: " <> unAirportCode code <> ".\n"
  InvalidModel code -> "Invalid model: " <> unModelCode code <> ".\n"

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
  <> "  " <> formatField False 3 (maybe "---" unAirportCode location)
  <> "\n"

