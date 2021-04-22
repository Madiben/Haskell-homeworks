--Mahdi Bentaleb ELKBLN solutions for practice 4

isAbundant :: Int -> Bool
isAbundant x = x < sum([ x  `div` n| n <-[1..x] , x `mod` n ==0 ])-x

--

average :: [Int] -> Float 
average x = (fromIntegral (sum x) ::Float) / (fromIntegral (length x) :: Float )
--
exchange :: Int ->Int 
exchange x = floor (fromIntegral x * 363.82)

--
isPrime :: Int -> Bool 
isPrime x = length [ n | n<-[2..x], x `mod` n ==0]==1
--
primes :: [Int]
primes = [n|n<-[2..], isPrime n ]

--5
dominos :: [(Int,Int)]
dominos = [ (a,b)|  a<-[0..6],b<-[0..6], a<=b]
--6 not finished
pairs :: [(Int,Int)]
pairs = [ (a,b)|  a<-[0..],b<-[0..],n<-[0..], a+b == n]
--7 
alphabet :: [(Int,Char)]
alphabet = zip [0..25]['a'..'z']
--8 
everyThird :: [Char]
everyThird = [b|(a,b)<-alphabet,mod (a + 1) 3 == 0]

--9
square ::Int ->Bool 
square a = sum[ 1 | x <- [0..a], x*x == a] /= 0

--10
courses :: [([Char], [([Char], [Char], [Char])])]
courses = [ ("Calculus", [("Simon", "Jones", "BDE91E"),("Barack", "Obama", "DDA3KX")]), ("Imperative Programming", [("Simon", "Marlow", "ALX1K0"), ("John", "Hughes", "BDE91E")]), ("Functional Languages", [("Philip", "Wadler", "ABCDE6"), ("Simon", "Thompson", "CDE560")])]


