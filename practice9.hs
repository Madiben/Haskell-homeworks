--Mahdi Bentaleb Practice 9 --
penultimate :: [Int] -> Int
penultimate (x:xs) 
    | length (x:xs) > 2 = penultimate xs
    | otherwise = x

addSeconds :: [Integer] -> Integer
addSeconds [] = 0
addSeconds [b] = 0
addSeconds [a,b,c] = b
addSeconds (x:y:xs) =y + addSeconds xs
--2
drop' ::Int  -> [a] ->[a]
drop' n x
    |n <=0 = x
drop' _ [] = []
drop' n (x:xs) 
    | n > length (x:xs) = []
    | n>0 = drop' (n-1) xs

--3
langAndRegion :: [Char] -> ([Char],[Char])
langAndRegion "" =("","")
langAndRegion (x:xs) 
    |x == '-' = ("",xs)
    | otherwise = (x : before ,after)

    where
        (before,after) = langAndRegion xs

--4
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:ax) (b:bx) = (a,b) : zip' ax bx

--5
unzip' ::[(a,b)] ->([a] ,[b])
unzip' [] = ([],[])
unzip' a = (map fst a, map snd a)

unzip''  ::[(a,b)] ->([a] ,[b])
unzip'' (p:ps)= (fst p : fst (unzip'' ps), snd p : snd (unzip'' ps) )
unzip'' [] = ([],[])

--6
empty:: [Char] -> [Int]
empty "" = []
empty a= result (zip [1..] (lines a))
    where
        result :: [(Int,String)] -> [Int]
        result [] = []
        result (x:xs)
            | snd x == "" = fst x : result xs
            |otherwise = result xs

--7
splitAt':: Int-> [a]->([a],[a])
splitAt' n x
    | n <= 0 = ([], x)
splitAt' _ [] =  ([], [])
splitAt' n x= (first,second)
    where
        first = take n x 
        second = drop n x

--8
nub' :: Eq a => [a] -> [ä]
nub' [] = []
