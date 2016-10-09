module Brain
  (
    Brain (..)
  , brainPredictG
  , brainUpdateG
  , createBrain
  , usePolicy
  , predictNextAction
  ) where

import NN
import GApprox
import Policy

-- A Brain is an agent's current policy and g-approximation
data Brain = Brain Network GApprox LearningRate deriving (Eq)
instance Show Brain where
  show _ = "This is the brain."  :: String

type LearningRate = Double

brainPredictG :: Brain -> [Double] -> IO Double
brainPredictG (Brain _ gfunc _) input = do
  list <- predictG gfunc input
  -- there should only be one value for G returned
  return $ head list

brainUpdateG :: Brain -> [Double] -> Double -> Brain
brainUpdateG (Brain net gfunc rate) input actualG = Brain net newG rate
  where newG = updateG gfunc rate input [actualG]

-- Creates a default Brain
-- Has no policy (policy is only an input layer)
-- Has a randomly generated neural network for G approximation, and it will train said network
-- with a learning rate of 0.5
createBrain :: Brain
createBrain = Brain (Network [Input]) createGApprox 0.5

-- Currently only supports Network (from NN.hs, not AI.HNN)
usePolicy :: Brain -> Network -> Brain
usePolicy (Brain _ g r) net = Brain net g r

predictNextAction :: Brain -> [Double] -> [Double]
predictNextAction (Brain net _ _) = predict net
