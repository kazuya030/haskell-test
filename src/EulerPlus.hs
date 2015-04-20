module EulerPlus where

import Control.Monad
import Control.Applicative

main = do
    ns <-  fmap (read :: String -> Integer) . lines <$> getContents
    mapM_ print $ map euler02 $ drop 1 ns

sumTo :: Integer -> Integer
sumTo n = n*(n+1) `div` 2

sumMultipleTo :: Integer -> Integer -> Integer
sumMultipleTo n k = k * sumTo ( n `div` k )

euler01 :: Integer -> Integer
euler01 n =  sumMultipleTo (n-1)  3 + sumMultipleTo (n-1) 5 - sumMultipleTo (n-1) 15

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

euler02 :: Integer -> Integer
euler02 n =  sum $ [ x | x <- takeWhile(<n) fibs, x `mod` 2==0]
