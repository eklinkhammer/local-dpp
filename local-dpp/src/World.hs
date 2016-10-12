module World
  (
    World (..)
  , getAgents
  , getObstacles
  , boundLocation
  , assignAgentsPolicies
  , stepAgents
  , broadcastG
  , calculateG
  ) where

import Location
import Agent
import Obstacle
import State
import NN
import Data.List

data World = World Agents Obstacles UpperRightCorner deriving (Show)

type Agents = [Agent]
type Obstacles = [Obstacle]
type UpperRightCorner = Location

getAgents :: World -> Agents
getAgents (World agents _ _) = agents

getObstacles :: World -> Obstacles
getObstacles (World _ obs _) = obs

getHeight :: World -> Double
getHeight (World _ _ (Location _ y)) = y

getWidth :: World -> Double
getWidth (World _ _ (Location x _)) = x

boundLocation :: World -> Location -> Location
boundLocation world (Location x y) = Location inX inY
  where
    inX = let width = getWidth world in if width > x then x else width
    inY = let height = getHeight world in if height > y then y else height
    

boundAgent :: World -> Agent -> Agent
boundAgent world (Agent (State loc or) s b) = Agent (State newLoc or) s b
  where newLoc = boundLocation world loc
  
stepAgent :: World -> Agent -> Agent
stepAgent world agent = boundAgent world $ step (getAgents world) (getObstacles world) agent

-- Move all agents one timestep
stepAgents :: World -> World
stepAgents w@(World agents obs bounds) = World (map (stepAgent w) agents) obs bounds

-- All agents receive the current value of G, and update their local approximations of G
broadcastG :: World -> World
broadcastG w@(World agents obs bounds) = World (map (receiveG agents obs (calculateG w)) agents) obs bounds 

calculateG :: World -> Double
calculateG (World agents obs _) = sum $ map (calculateObstacleScore agents) obs

calculateObstacleScore :: [Agent] -> Obstacle -> Double
calculateObstacleScore agents (POI loc reqAgents val minR) = result
  where
    agentDistances = map (distance loc . robLocation) agents
    validDistances = filter (< minR) agentDistances
    sortedDists    = sort validDistances
    result         = if (length validDistances) < reqAgents then 0
                     else 0.5 * val / (sum $ map (max 0.5) $ take reqAgents sortedDists)
    

assignAgentsPolicies :: [Network] -> World -> World
assignAgentsPolicies nets w@(World agents obs bound) = World (zipWith agentUsePolicy nets agents) obs bound
