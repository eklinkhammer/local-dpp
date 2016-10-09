module NN
  (
    Network (..)
  , Layer (..)
  , Weight (..)
  , initNetwork
  , computeNetwork
  , mutateNetwork
  ) where

-- Neural network for use in CCEA (no backprop).
-- Didn't want to muck with existing libraries to support random mutation of networks
import Policy

data Layer = Input
           | Layer Weights deriving (Eq)

e = exp 1
type Weights = [[Weight]]
data Network = Network [Layer] deriving (Eq)

data Weight = Zero | Value Double deriving (Eq)

multWeight :: Double -> Weight -> Double
multWeight _ Zero = 0.0
multWeight d (Value v) = d*v

-- previous layer produced a list of doubles. Output list of doubles
computeLayer :: Layer -> [Double] -> [Double]
computeLayer Input input = input
computeLayer (Layer weights) input = map (activationFunction . sum . zipWith multWeight input) weights

activationFunction :: Double -> Double
activationFunction t = 1 / (1 + e ** (-t))

computeNetwork :: Network -> [Double] -> [Double]
computeNetwork (Network []) input = input
computeNetwork (Network (top:rest)) input = computeNetwork (Network rest) $ computeLayer top input

instance Policy Network where
  predict = computeNetwork

initNetwork :: [Weights] -> Network
initNetwork weights = Network $ Input : (map Layer) weights

-- mutates a network's weights deterministically
-- TODO: Use Lens (somehow?)
mutateNetwork :: Network -- input network
              -> Int     -- Layer (input is 0)
              -> Int     -- Node
              -> Int     -- Wire
              -> Weight  -- New Weight
              -> Network
mutateNetwork (Network layers) layer node wire weight = Network $ replaceList newLayer layer layers
  where
    newLayer      = Layer $ replaceList newNode node listL
    newNode       = replaceList weight wire oldNode
    (Layer listL) = (layers !! layer)
    oldNode       = (listL !! node)

replaceList :: a -> Int -> [a] -> [a]
replaceList elem index xs = let (ys,zs) = splitAt index xs
                            in if length zs > 1 then ys ++ [elem] ++ (tail zs)
                               else ys ++ [elem]
