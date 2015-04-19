module Math where

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)