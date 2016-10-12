module Runner
  (
    startExperiment
  , FitnessFunction (..)
  ) where

import CCEA
import Simulation
import World
import NN
import Location
import System.Random
import Obstacle
import Agent

-- init N populations of K NN
-- init 2N agents, world

-- Foreach Population
--  mutate k -> 2k
--  foreach 1 -> 2k
--    select policy from each population
--    assign to agent
--    run simulation in world
--    receive a value for D
--  choose k policies

createWorld :: Double -> Int -> Int -> Int -> IO World
createWorld sideLength agentCount maxAgents numPOIs = do
  agents <- randomList agentCount (randomAgent sideLength)
  obstacles <- randomList numPOIs (randomObstacle sideLength maxAgents 10)
  return $ World agents obstacles (Location sideLength sideLength)
  

createNN :: IO Network
createNN = newNetwork 8 2 10 0.3

createNNs :: Int -> IO [Network]
createNNs k = sequence $ map (\_ -> createNN) $ [1..k]

createPop :: Int -> IO Population
createPop k = do
  networks <- createNNs k
  let netsWithFit = map (\n -> (n,1.0)) networks
  return netsWithFit

createPops :: Int -> Int -> IO [Population]
createPops n k = sequence $ map (\_ -> createPop k) $ [1..n] 
  
type Population = [(Network,Fitness)]


startExperiment :: Int             -- N
                -> Int             -- k
                -> Int             -- number of generations
                -> Double          -- World size
                -> Int             -- steps per simulation
                -> Int             -- max required agents for reward
                -> Int             -- numPOIs
                -> Double          -- Epsilon for CCEA selection
                -> FitnessFunction -- Dpp / D / local / global
                -> IO Double       -- The highest G achieved by a team
startExperiment n k gens worldSize steps maxAgents numPOIs epsilon fitFunc = do
  putStrLn "Initializing World..."
  startingWorld  <- createWorld worldSize n maxAgents numPOIs
  putStrLn $ show startingWorld
  putStrLn "Initializing populations..."
  startingPops   <- createPops n k
--  putStrLn $ concatMap show startingPops
  putStrLn "Starting experiments..."
  (_,world)      <- runExperiment gens epsilon steps k startingWorld fitFunc startingPops
  return $ calculateG world
  
debugExperiment :: ([Population],World) -> IO ()
debugExperiment = putStrLn . show . calculateG . snd

runExperiment :: Int    -- Number of generations
              -> Double -- epsilon for CCEA selection
              -> Int    -- steps per simulation
              -> Int    -- size of population pool
              -> World -> FitnessFunction -> [Population] -> IO ([Population],World)
runExperiment 0 _ _ _ w _ population = return (population,w)
runExperiment gens epsilon steps sizePool world fit population = do
  --putStrLn $ concatMap show population
  (thisGen,newW) <- runGeneration epsilon steps sizePool world fit population
  let g          =  calculateG newW
  --putStrLn $ show newW
  --putStrLn $ concatMap show thisGen
  putStrLn $ "Generation count: " ++ show gens ++ " Reward: " ++ show g
  nextGen        <- runExperiment (gens - 1) epsilon steps sizePool newW fit thisGen
  return nextGen

  
runGeneration :: Double -- epsilon for CCEA selection
              -> Int    -- steps in simulation
              -> Int    -- k (size of population pool)
              -> World -> FitnessFunction -> [Population] -> IO ([Population],World)
runGeneration e steps k w fit pops = do
  putStrLn $ "runGeneration pops: " ++ concatMap show pops
  pools                <- sequence $ map get2kNetworks pops
  putStrLn $ "Pools: " ++ concatMap show pools
  (scoredPool,world)   <- simulateTeams (evaluateNetworkTeam steps fit) w pools
  selectedPool         <- sequence $ map (\population -> epsilonSelectK population e k) scoredPool
  return (selectedPool,world)

evaluateNetworkTeam :: Int -> FitnessFunction -> World -> [Network] -> IO ([Fitness],World)
evaluateNetworkTeam n fit w nets = do
  (fitness, newW) <- policyScore n w nets fit
  return $ (map doubleToFitness fitness, newW)

doubleToFitness :: Double -> Fitness
doubleToFitness = id
main = putStrLn ""

randomLocation :: Double -> IO Location
randomLocation bound = do
  x <- randomRIO (0.0,bound)
  y <- randomRIO (0.0,bound)
  return $ Location x y

randomObstacle :: Double -> Int -> Double -> IO Obstacle
randomObstacle locBound maxAgents maxValue = do
  loc    <- randomLocation locBound
  agents <- randomRIO (1,maxAgents)
  value  <- randomRIO (0,maxValue)
  reward <- randomRIO ((locBound / 5), locBound / 2)
  return $ POI loc agents value reward

randomAgent :: Double -> IO Agent
randomAgent locBound = do
  loc <- randomLocation locBound
  return $ createAgent loc

randomList :: Int -> IO a -> IO [a]
randomList count generator = do
  list <- sequence $ map (\_ -> generator) [1..count]
  return list
