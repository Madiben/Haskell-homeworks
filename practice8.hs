allPositive :: [Int] -> Bool
allPositive [] = True 
allPositive (first:rest)
    | first <= 0 = False 
    | otherwise  = allPositive rest

lowerCase :: [Char] -> [Char]
lowerCase [] = []
lowerCase (c:cs)
    | c `elem` [n|n<-['a'..'z']] = c : lowerCase cs 
    | otherwise = lowerCase cs

--2
last' :: [Int] -> Int 
last' [n] = n
last' (x:xs) =last' xs
--3
and' :: [Bool] -> Bool
and' [] = True 
and' (x:xs)
    |not x =False 
    | otherwise = and' xs

--4
or' :: [Bool] -> Bool
or' [] = False 
or' (x:xs)
    | x && ( or' xs || not (or' xs)) = True
    | not x &&  or' xs = True 
    | otherwise = False 

replicate' :: Int -> Char -> [Char]
replicate' n c 
    | n > 0 = c : replicate' (n-1) c
    | otherwise = ""

format :: Int -> [Char] -> [Char]
format n c 
    | n < length c = c    
    | otherwise = ' ': format (n-1) c 

insert :: Int -> [Int] -> [Int]
insert n []=[n]
insert n (x:xs)
    | n <= x = n : (x:xs)
    | otherwise = x: insert n xs

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insert x (sort xs)

merge :: [Int] -> [Int]-> [Int]
merge x [] = x
merge [] c = c 
merge (x:xs) c = merge xs (insert x c )

mergeSort ::  [Int] -> [Int]
mergeSort [] = []
mergeSort x = merge (sort (take (length x `div` 2) x)) (sort (drop (length x `div` 2) x))

--11
breakOn :: Char -> [Char] -> ([Char], [Char])
breakOn c "" = ("","")
breakOn a (x:xs) 
    | a == x = ("",x:xs)
    | otherwise = ( x : before , after )
    
    where 
        (before, after) = breakOn a xs

--12
splitOn :: Char -> [Char] -> [String]
splitOn _ [] = []
splitOn a word 
    | second == "" = [first] 
    | otherwise = first : splitOn a (tail second)
    where
        (first,second) = breakOn a word

--13
csv :: String ->[[String ]]
csv s = [splitOn ',' line | line <- splitOn '\n' s]