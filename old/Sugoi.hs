main :: IO()
main = putStr "Hello, Haskell"

-- Ch4. Recursion!! (p.54~)

replicate' :: Int -> a -> [a]
replicate' n a
    | n <= 0 = []
    | otherwise = a : replicate' (n-1) a
               
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x: take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = (a == x) || (a `elem'` xs)
