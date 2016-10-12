module Location
  (
    Location (..)
  , distance
  , angle
  ) where

data Location = Location XPos YPos deriving (Show, Eq)

type XPos = Double
type YPos = Double

distance :: Location -> Location -> Double
distance (Location x1 y1) (Location x2 y2) = min 0.01 (sqrt $ square (x2 - x1) + square (y2 - y1))
  where square x = x * x

angle :: Location -> Location -> Double
angle (Location x1 y1) (Location x2 y2) = let (y,x) = (y2 - y1, x2 - x1)
                                          in if x == 0 then pi / 2 else tan $ (y2 - y1) / (x2 - x1)

addLoc :: Location -> Location -> Location
addLoc (Location x y) (Location a b) = Location (x + a) (b + y)
