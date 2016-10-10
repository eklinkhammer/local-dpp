module CCEA
  (
    Fitness
  , get2kNetworks
  , simulateTeams
  , epsilonSelectK
  ) where

import Policy
import System.Random
import NN
import Agent
import World

type Fitness = Double

-- A module for Cooperative Coevolutionary Algorithms

-- for a given population with k networks, generate 2k networks, with the additional k
-- being mutations of the original k
get2kNetworks :: [(Network,Fitness)] -> IO [(Network,Fitness)]
get2kNetworks pop = do
  successors <- sequence $ map mutatePolicy pop
  return $ pop ++ successors

-- Given a set of populations each with their own set of neural networks, choose one at random
-- from each population (without replacement) to be on a team.
-- Evaluate that team according to the performance of some team of agents
-- Result is the list of policies inputted with new fitness values (based on simulation)
simulateTeams :: (World -> [Network] -> IO ([Fitness],World)) -> World -> [[(Network,Fitness)]] -> IO ([[(Network,Fitness)]],World)
simulateTeams _  w          [] = return ([],w)
simulateTeams evaluateTeam w pops = do
  (team,futureTeams) <- generateTeam pops
  (scores,world)     <- evaluateTeam w $ map fst team
  let scoredTeam     = zipWith assignFitness scores team
  (resultFutureTeams,newW)  <- simulateTeams evaluateTeam world futureTeams
  return $ (returnTeam (scoredTeam, resultFutureTeams),newW)

mutatePolicy :: (Network,Fitness) -> IO (Network,Fitness)
mutatePolicy (net,fit) = do
  newNet <- mutateNetworkIO net
  return (newNet,fit)

assignFitness :: (Policy p) => Fitness -> (p,Fitness) -> (p,Fitness)
assignFitness newF (p,f) = (p,newF)

returnTeam :: ([a],[[a]]) -> [[a]]
returnTeam (ms,tms) = zipWith (:) ms tms

--Pulls from the list of populations a team with one member from each of the input populations
-- Returns the team and the list of populations sans selected team member
generateTeam :: [[a]] -> IO ([a],[[a]])
generateTeam listPops = do
  chosenAndRest <- sequence $ map removeRandom listPops
  let twoLists  = tupleListFromListTuples chosenAndRest
  return twoLists
  
tupleListFromListTuples :: [(a,b)] -> ([a],[b])
tupleListFromListTuples = foldr appendTuple ([],[])

appendTuple :: (a,b) -> ([a],[b]) -> ([a],[b])
appendTuple (a,b) (as,bs) = (a:as,b:bs)

epsilonSelectK :: (Ord a, Fractional a) => [(p,a)] -> Double -> Int -> IO [(p,a)]
epsilonSelectK source epsilon k = do
  (selected,rest) <- epsilonSelect source epsilon
  next            <- if k == 1 then return [selected]
                     else fmap (selected:) $ epsilonSelectK rest epsilon (k - 1)
  return next

epsilonSelect :: (Ord a, Fractional a) => [(p,a)] -> Double -> IO ((p,a),[(p,a)])
epsilonSelect source epsilon = do
  rand            <- randomRIO (0.0,1.0) :: IO Double
  (selected,rest) <- if rand > epsilon then removeRandom source
                     else return $ removeMax source
  return (selected, rest)

-- unsafe removeN
removeN :: [a] -> Int -> (a,[a])
removeN xs n = let (ys,zs) = splitAt n xs
               in   (head zs, ys ++ (tail zs))

removeMax :: (Ord a, Fractional a) => [(p,a)] -> ((p,a),[(p,a)])
removeMax xs = removeN xs $ maxFit xs 0.0
                                
maxFit :: (Ord a) => [(p,a)] -> a -> Int
maxFit rest currentMax = let val = snd $ head rest
                         in if val > currentMax then maxFit (tail rest) val
                            else maxFit (tail rest) currentMax

randomListInt :: [a] -> IO Int
randomListInt list = randomRIO (0, (length list) - 1)

chooseRandom :: [a] -> IO a
chooseRandom list = fmap (list !!) $ randomListInt list

removeRandom :: [a] -> IO (a,[a])
removeRandom list = fmap (removeN list) $ randomListInt list
