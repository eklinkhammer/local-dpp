module Agent where

import Location

data Agent = Agent State Sensors Brain deriving (Show, Eq)

data State = State Location Orientation deriving (Show, Eq)

type Orientation = Angle
type Angle = Double

data Brain = Brain deriving (Eq)
instance Show Brain where
  show _ = "" :: String

type Sensors = [Sensor]
data Sensor = Sensor deriving (Eq)
instance Show Sensor where
  show = const ""
