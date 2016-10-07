module Sensor
  (
    Sensor (..)
  , Quad (..)
  , scoreSensor
  ) where

import Location
import State
import Obstacle

data Quad = NW | SW | NE | SE deriving (Show, Eq)

data Sensor = Sensor Quad deriving (Eq)
instance Show Sensor where
  show (Sensor q) = show q


-- scores a single other location, per a location scoring function, for a given sensor and state
scoreSensor :: Sensor -> State -> Location -> (Location -> Double) -> Double
scoreSensor sensor robState@(State loc _) target scoringFunction = if isInQuadrant sensor robState target then scoringFunction loc else 0

isInQuadrant :: Sensor -> State -> Location -> Bool
isInQuadrant (Sensor q) (State location orient) targetLoc = upperAngle >= targetAngle && lowerAngle <= targetAngle
  where
    upperAngle = clampToUnitCircle $ rotate upper
    lowerAngle = clampToUnitCircle $ rotate lower
    rotate = (+) orient
    (upper, lower) = angleRange q
    targetAngle = angle location targetLoc
    

angleRange :: Quad -> (Double, Double)
angleRange NW = (pi/2, pi)
angleRange SW = (pi, 3*pi/2)
angleRange NE = (0, pi/2)
angleRange SE = (3*pi/2, 2*pi)

clampToUnitCircle :: Double -> Double
clampToUnitCircle angle = if angle > 2*pi then clampToUnitCircle (angle - 2*pi) else angle
