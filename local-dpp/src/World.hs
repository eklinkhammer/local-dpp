module World
  (
    World (..)
  , getAgents
  , getObstacles
  , boundLocation
  ) where

import Location
import Agent (Agent (..))
import Obstacle

data World = World Agents Obstacles UpperRightCorner

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
    

stepAgent :: World -> Agent -> Agent
stepAgent world agent = boundLocation $ step (getAgents world) (getObstacles world) agent

-- Move all agents one timestep
stepAgents :: World -> World
stepAgents w@(World agents obs bounds) = World (map (stepAgent w) agents) obs bounds

broadcastG :: World -> World
broadcastG w@(World agents obs bounds) = World (map (receiveG (calculateG w)) agents) obs bounds 

calculateG :: World -> Double
calculateG = undefined

