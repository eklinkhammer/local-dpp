module Main where

import Runner
import System.Environment
import System.Exit


n = 1
k = 1
gens = 2
size = 1
steps = 150
epsilon = 0.8
reqAgents = 1
fitFunc = DGlobal
numPOIS = 1

main = do
  x <- startExperiment n k gens size steps reqAgents numPOIS epsilon fitFunc
  putStrLn $ show x
