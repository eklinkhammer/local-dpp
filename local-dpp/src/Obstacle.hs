module Obstacle where

import Location

data Obstacle = Obstacle Location Radius
              | POI Location VisibleRadius Points
              | Wall Location Location
              deriving (Show)

type Radius = Double
type VisibleRadius = Double
type Points = Double
