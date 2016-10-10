module NN
  (
    Network (..)
  , Layer (..)
  , Weight (..)
  , initNetwork
  , computeNetwork
  , mutateNetwork
  , mutateNetworkIO
  , newNetwork
  ) where

-- Neural network for use in CCEA (no backprop).
-- Didn't want to muck with existing libraries to support random mutation of networks
import Policy
import System.Random

data Layer = Input
           | Layer Weights deriving (Eq, Show)

e = exp 1
type Weights = [[Weight]]
data Network = Network [Layer] deriving (Eq, Show)

data Weight = Zero | Value Double deriving (Eq, Show)

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

accessWeight :: Network -> Int -> Int -> Int -> Weight
accessWeight (Network layers) layer node wire = let (Layer l) = (layers !! layer) in (l !! node) !! wire

replaceList :: a -> Int -> [a] -> [a]
replaceList elem index xs = let (ys,zs) = splitAt index xs
                            in if length zs > 1 then ys ++ [elem] ++ (tail zs)
                               else ys ++ [elem]

-- With equal probability, change one node by 
mutateNetworkIO :: Network -> IO Network
mutateNetworkIO network = do
  bigSmall <- randomBool
  newNet   <- if bigSmall then tweakLarge network else tweakSmall network >>= tweakSmall >>= tweakSmall
  return newNet

tweakLarge :: Network -> IO Network
tweakLarge net = do
  (a,b,c) <- getRandomCoord net
  upDown <- randomBool
  let currentWeight = accessWeight net a b c
  let newWeight = case currentWeight of
                    Zero      -> Zero
                    (Value d) -> if upDown then Value (d * 1.5) else Value (d * 0.6)
  return $ mutateNetwork net a b c newWeight
 

getRandomCoord :: Network -> IO (Int,Int,Int)
getRandomCoord (Network layers) = do
  layerI         <- randomListInt layers
  let (Layer listL) = layers !! layerI
  nodeI <- randomListInt listL
  let node = listL !! nodeI
  wireI <- randomListInt node
  let wire = node !! wireI
  return (layerI, nodeI, wireI)
  

tweakSmall :: Network -> IO Network
tweakSmall net = do
  (a,b,c) <- getRandomCoord net
  val <- randomRIO (-0.1,0.1)
  let currentWeight = accessWeight net a b c
  let newWeight = case currentWeight of
                    Zero -> Zero
                    (Value d) -> Value (d + val)
  return $ mutateNetwork net a b c newWeight

randomBool :: IO Bool
randomBool = do
  r <- randomRIO (0,1) :: IO Int
  return $ r*2 > 1

-- TODO: Put in util class, this code is duplicated
randomListInt :: [a] -> IO Int
randomListInt list = randomRIO (0, (length list) - 1)

-- randomly generate a weight value
randomWeight :: Double -> IO Weight
randomWeight frequencyWire = do
  exists <- randomRIO (0.0,1.0)
  value  <- randomRIO (0.0,1.0)
  return $ if exists > frequencyWire then Value value else Zero

randomListWeight :: Double -> [a] -> IO [Weight]
randomListWeight freq l = sequence $ map (\_ -> randomWeight freq) l

-- creates a new network with one hidden layer
newNetwork :: Int -> Int -> Int -> Double -> IO Network
newNetwork inputs outputs hiddenCount wireFrequency = do
  let firstHidden = map (\_ -> [1..inputs]) [1..hiddenCount]
  hLayer1         <- sequence $ map (randomListWeight wireFrequency) firstHidden
  let output      = map (\_ -> [1..hiddenCount]) [1..outputs]
  outputLayer     <- sequence $ map (randomListWeight wireFrequency) output
  return $ initNetwork [hLayer1,outputLayer]
  
