module Policy
  (
    Policy (..)
  ) where

class Policy a where
  predict :: a -> [Double] -> [Double]
