module Math where

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = 2:sieve [3, 5..]
    where sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /=0]
