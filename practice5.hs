--s
--Mahdi Bentaleb ELKBLN solutions for practice 5`
square :: Int -> Bool
square n= sum [1 | x <-[0..n],x * x == n] /=0

powersOfTwo :: [Integer]
powersOfTwo = [2^x| x<-[0..]]

-- practice 11
--1
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show,Enum)
--2
isFirstDayOfWeek :: Day -> Bool
isFirstDayOfWeek Monday = True 
isFirstDayOfWeek _ = False 
--3
isWeekend :: Day -> Bool 
isWeekend Saturday = True 
isWeekend Sunday = True 
isWeekend _ = False 
--4
next :: Day -> Day
next Sunday = Monday
next d = succ d
--5
data Time = T Int Int 
--6
showTime :: Time -> String
showTime (T h m)= show h ++ "." ++ show m 
--7
eqTime :: Time -> Time -> Bool 
eqTime (T h1 m1) (T h2 m2)= (h1==h2)&&(m1==m2)
--8
isEarlier :: Time -> Time -> Bool 
isEarlier (T h1 m1) (T h2 m2) = (h2>h1)||(h1 == h2 && m2 > m1)
--9
isBetween :: Time -> Time -> Time -> Bool 
isBetween t1 t2 t3 = (isEarlier t1 t2 && isEarlier t2 t3) || (isEarlier t3 t2 && isEarlier t2 t1)
--9 2
time :: Int -> Int-> Time 
time h m
    | h < 0 || h >= 24 = error ("time: invalid hour: "++ show h)
    | m < 0 || m >= 60 = error ("time: invalid minute: "++ show m)
    |otherwise = T h m
--10
data USTime = AM Int Int | PM Int Int
    deriving(Show, Eq)
--11
showUSTime :: USTime -> String
showUSTime (AM h m) = show h ++ "." ++ show m ++ " am"
showUSTime (PM h m) = show h ++ "." ++ show m ++ " pm"

--12
usTimeToTime :: USTime -> Time
usTimeToTime (AM h m) 
    | h == 12 = T 0 m
    | otherwise = T h m
usTimeToTime (PM h m)
    | h == 12 = T 12 m
    | h <= 12 = T (h+12) m

--13
timeToUSTime :: Time -> USTime
timeToUSTime (T h m)
    | h < 12 && h /= 0 = AM h m 
    | h == 0 = AM 12 m 
    | h == 12 = PM 12 m
    | h > 12 = PM (h-12) m 
 

