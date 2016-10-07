module State
  (
    State (..)
  ) where

import Location

data State = State Location Orientation deriving (Show, Eq)
type Orientation = Angle
type Angle = Double
