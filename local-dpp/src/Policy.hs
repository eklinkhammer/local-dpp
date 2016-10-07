module Policy
  (
    Policy (..)
  ) where

class Policy a where
  predict :: a -> b -> c
