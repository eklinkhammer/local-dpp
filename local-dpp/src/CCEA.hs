module CCEA
  (

  ) where

import Policy
import System.Random

type Fitness = Double

-- A module for Cooperative Coevolutionary Algorithms

get2kNetworks :: (Policy p) => [(p,Fitness)] -> [(p,Fitness)]
get2kNetworks pop = pop ++ successors
  where successors = map mutatePolicy pop

simulateTeams :: (Policy p) => [[(p,Fitness)]] -> IO [[(p,Fitness)]]
simulateTeams [] = return []
simulateTeams pops = do
  (team,futureTeams) <- generateTeam pops
  let score          = evaluateTeam $ map fst team
  let scoredTeam     = map (assignFitness score) team
  resultFutureTeams  <- simulateTeams futureTeams
  return $ returnTeam (scoredTeam, resultFutureTeams)
  

initPolicy :: (Policy p) => (p,Fitness)
initPolicy = undefined

mutatePolicy :: (Policy p) => (p,Fitness) -> (p,Fitness)
mutatePolicy = undefined

assignFitness :: (Policy p) => Fitness -> (p,Fitness) -> (p,Fitness)
assignFitness newF (p,f) = (p,newF)

evaluateTeam :: (Policy p) => [p] -> Fitness
evaluateTeam = undefined


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
