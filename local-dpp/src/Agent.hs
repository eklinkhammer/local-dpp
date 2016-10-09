module Agent
  (
    Agent (..)
  , step
  ) where

import Location
import Sensor
import State
import Obstacle
import Brain
import NN (Network (..))

data Agent = Agent State Sensors Brain deriving (Show, Eq)

type Sensors = [Sensor]

-- A step for an agent in a simulation is the agent picking an action based on its policy
-- Out of world movements (ie, finite world) is dealt with by caller of step in boundLocation
step :: [Agent] -> [Obstacle] -> Agent -> Agent
step agents obstacles agent@(Agent state s brain) = move agent newLoc
  where
    newLoc = Location dx dy
    (dx:dy:_) = predictNextAction brain vars
    vars = getStateVariables agent obstacles agents
  



move :: Agent -> Location -> Agent
move (Agent (State loc _) sensors brain) newLoc = Agent state sensors brain
  where state = State newLoc $ angle loc newLoc

rotate :: Agent -> Double -> Agent
rotate (Agent (State loc orient) sensors brain) dif = Agent state sensors brain
  where state = State loc $ clampToUnitCircle ((clampToUnitCircle dif) + orient)

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


calcReward :: Agent -> Double -> Double
calcReward (Agent _ _ brain) g = (getRewardFunction brain) g

-- getRewardFunction will pattern match on the different kinds of brains (greedy, D, D++, local)
getRewardFunction :: Brain -> Double -> Double
getRewardFunction brain = undefined

agentPredictG :: Agent -> [Double] -> IO Double
agentPredictG (Agent _ _ brain) = brainPredictG brain

agentUpdateG :: Agent -> [Double] -> Double -> Agent
agentUpdateG (Agent state sens brain) x target = Agent state sens $ brainUpdateG brain x target

agentUsePolicy :: Agent -> Network -> Agent
agentUsePolicy (Agent state sens brain) net = Agent state sens $ usePolicy brain net


createAgent :: Location -> Agent
createAgent loc = Agent (State loc 0) sensors createBrain

receiveG :: Double -> Agent -> Agent
receiveG = undefined
