module Agent
  (
    Agent (..)
  , getStateVariables
  , move
  ) where

import Location
import Sensor
import State
import Obstacle

data Agent = Agent State Sensors Brain deriving (Show, Eq)

data Brain = Brain deriving (Eq)
instance Show Brain where
  show _ = "" :: String

type Sensors = [Sensor]

move :: Agent -> Location -> Agent
move (Agent (State loc _) sensors brain) newLoc = Agent state sensors brain
  where state = State newLoc $ angle loc newLoc

-- The state variables used in training the network. The four quadrant values for POIs and other
-- robots (respectively). 
getStateVariables :: Agent -> [Obstacle] -> [Agent] -> [Double]
getStateVariables agent obs agents = getStateVarsObs agent obs ++ getStateVarsAgents agent agents

getStateVarsObs :: Agent -> [Obstacle] -> [Double]
getStateVarsObs (Agent state sensors _) objs = map (\s -> sum $ map (\obj -> scoreSensor s state (getLocation obj) (obstacleScoring obj)) objs) sensors

obstacleScoring :: Obstacle -> Location -> Double
obstacleScoring obs loc = (getValue obs) / (distance loc $ getLocation obs)

getStateVarsAgents :: Agent -> [Agent] -> [Double]
getStateVarsAgents (Agent state sensors _) agents = map (\s -> sum $ map (\agent -> scoreSensor s state (robLocation agent) (robotScoring agent)) agents) sensors

robotScoring :: Agent -> Location -> Double
robotScoring rob loc = let robLoc = robLocation rob
                           in if robLoc == loc
                              then 0
                              else 1 / (distance robLoc loc)

robLocation :: Agent -> Location
robLocation (Agent (State loc _) _ _) = loc
