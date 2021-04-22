----Mahdi Bentaleb ELKBLN solutions for practice 3
--quiz 1
add4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
add4 (a,b,c,d) (e,f,g,h) = ( a + e , b + f , c + g , d + h )
--quiz 2
division :: (Int, Int) -> (Int, Int)
division a = (fst a `div` snd a , fst a `mod` snd a)

isLetter :: Char -> Bool 
isLetter a = elem a ['a'..'z'] || elem a ['A'..'Z']

isDigit :: Char  -> Bool 
isDigit a = elem a ['0'..] && isLetter a == False 
--8
mountain :: Int -> [Int]
mountain a = [1..a] ++ (reverse [1..a-1])
--9
divisors ::Int ->[Int]
divisors a = [ a  `div` n| n <-[1..a] , a `mod` n ==0 ]
--10
powerofTwo :: Int->[Int]
powerofTwo a = [2 ^ n| n<-[0..a-1]]
--11
--exercise: (-1) ^ n * (2 * n + 1)

--12
time :: [(Int,Int)]
time = [ (h,m)|  h<-[0..23],m<-[0..59]]
