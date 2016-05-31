
module Types.Time
  ( Minutes(..)
  , Weeks(..)
  , AbsoluteTime(..)
  , incrAbsoluteTime
  , TimeOfWeek()
  , timeOfWeek
  , DayOfWeek(..)
  , TimeOfDay()
  , timeOfDay
  , unpackAbsoluteTime
  , packAbsoluteTime
  , unpackTimeOfWeek
  , packTimeOfWeek
  ) where

import Control.Lens
import H.Prelude
import Prelude (fromIntegral)

newtype Minutes = Minutes { unMinutes :: Integer }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Show)

newtype Weeks = Weeks { unWeeks :: Integer }
  deriving (Eq, Ord, Num, Real, Integral, Enum, Show)

newtype AbsoluteTime = AbsoluteTime { absOffset :: Minutes }
  deriving (Eq, Ord, Show)

incrAbsoluteTime :: AbsoluteTime -> AbsoluteTime
incrAbsoluteTime AbsoluteTime{..} = AbsoluteTime $ absOffset + 1

newtype TimeOfWeek = TimeOfWeek { towOffset :: Minutes }
  deriving (Eq, Ord, Show)

timeOfWeek :: Minutes -> TimeOfWeek
timeOfWeek n = case n >= 0 && n < Minutes minutesPerWeek of
  True  -> TimeOfWeek n
  False -> error "TimeOfWeek out of range."

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq, Ord, Enum, Bounded, Show)

minutesPerDay :: Integer
minutesPerDay = 60 * 24

minutesPerWeek :: Integer
minutesPerWeek = minutesPerDay * 7

newtype TimeOfDay = TimeOfDay { todOffset :: Minutes }
  deriving (Eq, Ord, Show)

timeOfDay :: Minutes -> TimeOfDay
timeOfDay n = case n >= 0 && n < Minutes minutesPerDay of
  True  -> TimeOfDay n
  False -> error "TimeOfDay out of range."

unpackAbsoluteTime :: AbsoluteTime -> (Weeks, TimeOfWeek)
unpackAbsoluteTime AbsoluteTime{..} =
  over _1 Weeks . over _2 (TimeOfWeek . Minutes)
    $ unMinutes absOffset `divMod` minutesPerWeek

packAbsoluteTime :: Weeks -> TimeOfWeek -> AbsoluteTime
packAbsoluteTime Weeks{..} TimeOfWeek{..} =
  AbsoluteTime $ Minutes (unWeeks * minutesPerWeek) + towOffset

unpackTimeOfWeek :: TimeOfWeek -> (DayOfWeek, TimeOfDay)
unpackTimeOfWeek TimeOfWeek{..} =
  over _1 (toEnum . fromInteger) . over _2 (TimeOfDay . Minutes)
    $ unMinutes towOffset `divMod` minutesPerDay

packTimeOfWeek :: DayOfWeek -> TimeOfDay -> TimeOfWeek
packTimeOfWeek dow tod =
  TimeOfWeek $ Minutes (fromIntegral (fromEnum dow) * minutesPerDay) + todOffset tod

