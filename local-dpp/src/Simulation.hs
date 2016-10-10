module Simulation
  (
    FitnessFunction (..)
  , policyScore
  ) where

-- Simulation is the per policy simulation. It is used to evaluate the policies generated
-- by the CCEA algorithm.
import Agent
import World
import NN


data FitnessFunction = DppGlobal | DppLocal | DLocal | DGlobal

-- Simulates a single timestep
-- If the boolean value is true, that means the agents are using local approximations of G
-- This means that they must update their estimates of G using a broadcast value of G
simulationTimestep :: Bool -> World -> World
simulationTimestep broadcast world = f $ stepAgents world
  where f = if broadcast then broadcastG else id

simulationNSteps :: Int -> Bool -> World -> World
simulationNSteps 0 _ sim = sim
simulationNSteps n b sim = simulationNSteps (n - 1) b $ simulationTimestep b sim

-- randomize starting locations
initSim :: World -> [Network] -> World
initSim world nets = assignAgentsPolicies nets world

-- Returns a fitness score for the policies
-- Also returns the modified simulated world
policyScore :: Int -> World -> [Network] -> FitnessFunction -> IO ([Double],World)
policyScore n w nets fit = result
  where
    result = do
      agentsOut <- sequence $ map f agents
      return (agentsOut,simulation)
    simulation = simulationNSteps n b $ initSim w nets
    agents     = getAgents simulation
    g          = calculateG simulation
    (f,b) = case fit of
      DppLocal  -> (calculateDppLocal g simulation, True)
      DLocal    -> (calculateDLocal g simulation, True)
      DppGlobal -> (return . calculateDppGlobal g simulation, False)
      DGlobal   -> (return . calculateDGlobal g simulation, False)
   

calculateDGlobal :: Double -> World -> Agent -> Double
calculateDGlobal g w@(World agents obs corn)  agent = g - g'
  where g' = calculateG $ World (removeItem agent agents) obs corn

calculateDLocal :: Double -> World -> Agent -> IO Double
calculateDLocal g w@(World agents obs corn) agent = do
  let state = getStateVariables agent obs (removeItem agent agents)
  g'        <- agentPredictG agent state
  return (g - g')

calculateDppGlobal :: Double -> World -> Agent -> Double
calculateDppGlobal g w@(World agents obs corn) agent = max d dpp
  where
    d   = calculateDGlobal g w agent
    dpp = calculateDppHelper (length agents) w agent

calculateDppHelper :: Int -> World -> Agent -> Double
calculateDppHelper 0 _                         _     = 0
calculateDppHelper n w@(World agents obs corn) agent = max (calculateG w) (calculateDppHelper (n-1) counterfactual agent)
  where counterfactual = World (agent:agents) obs corn

calculateDppLocal :: Double -> World -> Agent -> IO Double
calculateDppLocal g w@(World agents obs corn) agent = do
  d   <- calculateDLocal g w agent
  dpp <- calculateDppLocalHelper (length agents) w agent
  return $ max d dpp


calculateDppLocalHelper :: Int -> World -> Agent -> IO Double
calculateDppLocalHelper 0 _                         _     = return 0
calculateDppLocalHelper n w@(World agents obs corn) agent = do
  let state = getStateVariables agent obs agents
  val       <- agentPredictG agent state
  maxVal    <- calculateDppLocalHelper (n-1) (World (agent:agents) obs corn) agent
  return $ max val maxVal

-- TODO - put in Util class
removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []     = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys 
