module EulerPlus where

import math
import Control.Monad
import Control.Applicative

sumTo :: Integer -> Integer
sumTo n = n*(n+1) `div` 2

sumMultipleTo :: Integer -> Integer -> Integer
sumMultipleTo n k = k * sumTo ( n `div` k )

solve :: Integer -> Integer
solve n =  sumMultipleTo (n-1)  3 + sumMultipleTo (n-1) 5 - sumMultipleTo (n-1) 15

main = do
    ns <-  fmap (read :: String -> Integer) . lines <$> getContents
    mapM_ print $ map solve $ drop 1 ns