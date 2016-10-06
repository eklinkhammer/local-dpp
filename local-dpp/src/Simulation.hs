module Simulation
  (
    test
  ) where

import Queue
import Location
import Agent

-- In the interest of time, this class is not a general framework for a robot simulation
-- Probably needs a lot of refactoring to be used as a general event driven simulator
-- Oh well. Realistically, grab a Russian library once I have the time to understand it.

test = putStrLn "Hello World!"

data Simulator = Simulator World EventQ ObjectiveFunction

type ObjectiveFunction = (World -> Double)
  
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
createAgent location orient = Agent (State location orient) ([] :: Sensors) Brain


getValue :: World -> Agent -> Double
getValue (World _ pois _) (Agent state sensors _) = undefined


