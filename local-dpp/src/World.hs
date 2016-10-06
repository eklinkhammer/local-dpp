module World where

import Location
import Agent
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
