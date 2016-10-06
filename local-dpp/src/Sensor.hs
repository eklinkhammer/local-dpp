module Sensor where

data Quad = NW | SW | NE | SE deriving (Show, Eq)

data Sensor = Sensor Quad

isInQuadrant :: Sensor -> State -> Location -> Bool
isInQuadrant (Sensor q) robotState robotLocation =

angleRange :: Quad -> (Double, Double)
angleRange NW = (pi/2, pi)
angleRange SW = (pi, 3*pi/2)
angleRange NE = (0, pi/2)
angleRange SE = (3*pi/2, 2*pi)

clampToUnitCircle :: Double -> Double
clampToUnitCircle angle = if angle > 2*pi then clampToUnitCircle (angle - 2*pi) else angle

