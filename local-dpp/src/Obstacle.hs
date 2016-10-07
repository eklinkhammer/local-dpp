module Obstacle
  (
    Obstacle (..)
  , getValue
  , getLocation
  ) where

import Location

data Obstacle = Obstacle Location Radius
              | POI Location VisibleRadius Value
              | Wall Location Location
              deriving (Show)

type Radius = Double
type VisibleRadius = Double
type Value = Double

getValue :: Obstacle -> Value
getValue (POI _ _ v) = v
getValue _           = 0

getLocation :: Obstacle -> Location
getLocation (POI loc _ _) = loc
getLocation _             = Location 0 0
