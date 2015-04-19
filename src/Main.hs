module Main where

import Math

euler01' :: Integer -> [Integer]
euler01' n = [ x | x <- [1..n-1], (x `mod` 3 == 0 || x `mod` 5 ==0)]

euler01 = sum $ euler01' 1000

euler02' :: Integer -> [Integer]
euler02' n = [x | x <- takeWhile(<n) fibs, x `mod` 2 == 0]

euler02 = sum $ euler02' 4000000

main = print $ euler02
