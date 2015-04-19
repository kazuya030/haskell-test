-- cf. すごいH p.22

main :: IO()
main = print $ triplet 240

triplet :: Integer -> [(Integer, Integer, Integer)]
triplet n = [(a,b,c) | c<-[1..n], a<-[1..c],b<-[1..a],a+b+c==2*n,a^2+b^2==c^2]

triplet' :: Integer -> [(Integer, Integer, Integer)]
triplet' n = [(a,b,c) | c<-[1..n], a<-[1..c],b<-[1..a],a^2+b^2==c^2, a+b+c==2*n]

