module NN
  (
    Network (..)
  ) where

import Policy

data Layer = Input
           | Layer Weights

e = exp 1
type Weights = [[Double]]
data Network = Network [Layer]

-- previous layer produced a list of doubles. Output list of doubles
computeLayer :: Layer -> [Double] -> [Double]
computeLayer Input input = input
computeLayer (Layer weights) input = map (activationFunction . sum . zipWith (*) input) weights

activationFunction :: Double -> Double
activationFunction t = 1 / (1 + e ** (-t))

computeNetwork :: Network -> [Double] -> [Double]
computeNetwork (Network []) input = input
computeNetwork (Network (top:rest)) input = computeNetwork (Network rest) $ computeLayer top input

instance Policy Network where
  predict = computeNetwork
