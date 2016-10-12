module HNNWrapper
  (

  ) where

import AI.HNN.FF.Network
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix
import Foreign.Storable
import System.Random
import qualified Data.Vector as V




randomizeMatrixValue :: V.Vector (Matrix a) -> IO (V.Vector (Matrix a))
randomizeMatrixValue = undefined

setMatrixValue :: (Element a) => (Int, Int) -> a -> Matrix a -> Matrix a
setMatrixValue (i,j) a matrix = fromRows newRows
  where
    oldRows = toRows matrix
    oldRow  = oldRows !! i
    newRow  = fromList $ replaceElement j a (toList oldRow)
    newRows = replaceElement i newRow oldRows
    
(!!+) :: Maybe [a] -> Int -> Maybe a
(!!+) Nothing     _ = Nothing
(!!+) (Just list) i = if i >= length list || i < 0 then Nothing else Just (list !! i)

replaceElement :: Int -> a -> [a] -> [a]
replaceElement i x xs = let (ys,zs) = splitAt i xs
                        in if length zs > 0 then ys ++ [x] ++ (tail zs)
                           else ys ++ [x]
