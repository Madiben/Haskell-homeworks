--Mahdi Bentaleb ELKBLN solutions for practice 2
--2
add' :: (Int,Int) -> (Int,Int) -> (Int,Int)
add' f1 f2 = ((fst f1 * snd f2)+(fst f2 * snd f1),(snd f1 * snd f2))
--3
mul' :: (Int,Int) -> (Int,Int) -> (Int,Int)
mul' f1 f2 = ((fst f1 * fst f2),(snd f1 * snd f2))
--4
modDiv' :: Int->Int->(Int,Int)
modDiv' a b = ((a `mod` b),( a `div` b))
--5
quadratic' :: Float -> Float -> Float ->( Float , Float )
quadratic' a b c = ( ( ( - b + sqrt ((b * b) - (4 * a * c) )) / (2*a) ) ,((-b - sqrt((b*b) - (4 * a * c))) / (2*a)))
--6
matches' :: (Int,Int)->(Int,Int)-> Bool
matches' a b = fst a == fst b || fst a == snd b || snd a == fst b || snd a == snd b
--7  
len' :: (Float,Float)->Float
len' a = sqrt (( fst a * fst a ) + ( snd a * snd a ) )
--8 
stretch' :: (Int,Int)->Int->(Int,Int)
stretch' a b = (fst a * b , snd a * b)
--9
distance' :: (Float,Float)->(Float,Float)->Float
distance' a b =  sqrt (( (fst b - fst a )^2) + (( snd b - snd a )^2) )
----Mahdi Bentaleb ELKBLN solutions for practice 3
--quiz 1
add4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
add4 (a,b,c,d) (e,f,g,h) = ( a + e , b + f , c + g , d + h )
--quiz 2
division :: (Int, Int) -> (Int, Int)
division a = (fst a `div` snd a , fst a `mod` snd a)