main :: IO()
main = putStr "Hello, Euler!"

euler1 :: Integer -> Integer
euler1 x = sum $ takeWhile(<x) [n | n<- [1..], n `mod` 3==0 || n `mod` 5==0]

fibs :: [Integer]
fibs = 1:2:zipWith (+) fibs (tail fibs)

euler2 :: Integer -> Integer
euler2 n = sum [x | x <- takeWhile(<n) fibs, even x]

primes :: [Integer]
primes = 2:sieve [3, 5..]
    where sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /=0]

facts :: Integer -> [Integer]
facts n = facts_in n 2

facts_in :: Integer -> Integer -> [Integer]
facts_in 1 _ = []
facts_in n p' = p:facts_in (n `div` p) p
    where 
      sqrt_n = floor $ sqrt $ fromIntegral n
      p_max = [x | x <- takeWhile(<=sqrt_n)primes, x>=p' , n `mod` x==0]
      p = if null p_max
            then n
            else head p_max

euler3 = last $ facts 600851475143

make_palin_descend :: Integer -> [Integer]
make_palin_descend n
    | n < 1 = []                  
    | otherwise = zipWith(*) le le
    where le = [10^n-1, 10^n-2..10^(n-1)]
