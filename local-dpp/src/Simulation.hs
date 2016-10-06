module Simulation
  (
    test
  ) where

import Queue

-- In the interest of time, this class is a general framework for a robot simulation
-- Probably needs a lot of refactoring to be used as a general event driven simulator
-- Oh well

test = putStrLn "Hello World!"

data Simulator = Simulator World EventQ ObjectiveFunction

type ObjectiveFunction = (World -> Double)
  
data World = World Agents Obstacles deriving (Show)

data Agent = Agent State Brain deriving (Show)
data State = State Location Orientation deriving (Show)
type Orientation = Angle
type Angle = Double

data Brain = Brain
instance Show Brain where
  show _ = "" :: String

data Location = Location XPos YPos deriving (Show)
type XPos = Double
type YPos = Double

data Obstacle = Obstacle Location Radius
              | POI Location Radius
              | Wall Location Location
              deriving (Show)

type Radius = Double


type Obstacles = [Obstacle]
type Agents = [Agent]
type Signal = Double

type EventQ = Queue Event
data Event = Train Agent Signal
           | Move Agent Command
           | Output
           | CalculateG

data Command = Straight Distance
             | Rotate Angle

type Distance = Double

instance Show Simulator where
  show (Simulator world _ _) = show world

simulateStep :: Simulator -> Simulator
simulateStep sim@(Simulator world events objf) =
  let (event, rest) = pop events
  in case event of
    Nothing  -> sim
    (Just e) -> executeEvent e (Simulator world rest objf)

addEvent :: Event -> Simulator -> Simulator
addEvent event (Simulator world events objf) = Simulator world pushed objf
  where pushed = push event events

executeEvent :: Event -> Simulator -> Simulator
executeEvent = undefined

createAgent :: Location -> Orientation ->  Agent
createAgent location orient = Agent (State location orient) Brain





