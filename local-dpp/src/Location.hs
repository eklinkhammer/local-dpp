module Location where

data Location = Location XPos YPos deriving (Show, Eq)

type XPos = Double
type YPos = Double

distance :: Location -> Location -> Double
distance (Location x1 y1) (Location x2 y2) = sqrt $ square (x2 - x1) + square (y2 - y1)
  where square x = x * x
