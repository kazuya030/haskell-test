module Main where

import Math

euler01' :: Integer -> [Integer]
euler01' n = [ x | x <- [1..n-1], (x `mod` 3 == 0 || x `mod` 5 ==0)]

euler01 = sum $ euler01' 1000

main = print $ take 10 fibs
