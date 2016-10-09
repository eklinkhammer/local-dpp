module GApprox
  (
    GApprox (..)
  , createGApprox
  , predictG
  , updateG
  ) where

-- Wrapper for HNN's Neural network module
-- Using it for function approximation while in simulation -> wrapper supports creation,
-- prediction, and update.
-- Usecase: Predict 1 -> update 1 -> predict 1 -> update 1

-- Goal -> understand library enough to support a CCEA approach (ie, be able to mutate the
-- weights). It seems like there is a load from weight matrix function, so should involve just
-- creating a Network -> Weight matrix function.

import AI.HNN.FF.Network
import Numeric.LinearAlgebra
import Policy

data GApprox = GApprox (IO (Network Double))
instance Eq GApprox where
  (==) _ _ = True

predictG :: GApprox     -- Neural Network Approximating G
         -> [Double]    -- Input (will be state action pair)
         -> IO [Double] -- predicted value of G
predictG (GApprox network) input = do
  net <- network
  return $ toList $ output net sigmoid $ fromList input

updateG :: GApprox -- Neural Network Approximating G
        -> Double   -- Learning Rate
        -> [Double] -- Input to G
        -> [Double] -- Actual value of G
        -> GApprox -- Updated Neural Network
updateG (GApprox network) rate input target = GApprox newNet
  where newNet = do
          net <- network
          let sample = [ fromList input --> fromList target ] :: Samples Double
          return $ trainNTimes 1 rate sigmoid sigmoid' net sample

createGApprox :: GApprox
createGApprox = GApprox $ createNetwork 8 [14] 1
