module Obstacle
  (
    Obstacle (..)
  , getValue
  , getLocation
  ) where

import Location

data Obstacle = Obstacle Location Radius
              | POI Location RequiredAgents Value MinRadius
              | Wall Location Location
              deriving (Show)

type Radius = Double
type Value = Double
type RequiredAgents = Int
type VisionRadius = Double
type MinRadius = Double

getValue :: Obstacle -> Value
getValue (POI _ _ v _) = v
getValue _           = 0

getLocation :: Obstacle -> Location
getLocation (POI loc _ _ _) = loc
getLocation _             = Location 0 0
