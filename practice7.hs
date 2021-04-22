--Mahdi Bentaleb ELKBLN solutions for practice 7
--Mahdi Bentaleb ELKBLN solutions for practice 7
import Data.Char ( toUpper, isLetter )

punctuation :: Char -> Bool
punctuation '.' = True 
punctuation '!' = True 
punctuation '?' = True 
punctuation _ = False 

firstTwo :: [Bool] -> Bool
firstTwo [] = False 
firstTwo [a] = a
firstTwo (x:c : rest)
    | (x && not c) || (not x && c) = True  
    | not x && not c = False  
    | x && c = False 


onAxis :: (Int, Int ) -> Bool 
onAxis (0,_) = True 
onAxis (_,0) = True 
onAxis (_,_) = False 

headInt :: [Int] -> Int
headInt (x : r) = x 

tailInt :: [Int] -> [Int]
tailInt (x : r) = r 

nullInt ::[Int] -> Bool 
nullInt [] = True
nullInt _ = False

isSingletonInt :: [Int] -> Bool 
isSingletonInt [first]=True 
isSingletonInt _ = False 

toUpperFirst :: String -> String
toUpperFirst ""=""
toUpperFirst (c:cs) = toUpper c : cs

isLetter:: Char -> Bool
isLetter a =  a `elem`  ['a'..'z'] || a `elem`  ['A'..'Z']
--5-6 
range :: Int -> Int -> [Int]
range a b   
    | a == b = [b]
    | a < b =  a: range (a+1) b
    | a > b =  a: range (a-1) b
--9
everySecond:: String -> String
everySecond ""=""
everySecond (a:"")=""
everySecond (a:b:c)= b : everySecond c

--10
elem' :: Char -> String -> Bool 
elem' s "" = False 
elem' a (c:d) = a == c || elem' a d

myLengthTailRec :: [Char] -> Int
myLengthTailRec list = loop list 0
    where
    loop :: [Char] -> Int -> Int
    loop [] len = len
    loop (_:xs) len = loop xs (len + 1)
myLength :: [Char] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMinimum :: [Int] -> Int
myMinimum [n] = n
myMinimum (n:ns)
    | n <= m = n
    | otherwise = m
    where
    m = myMinimum ns

myMinimcum :: [Int] -> Int
myMinimcum [n] = n
myMinimcum (n:ns) = min n (myMinimum ns)

myMinimumTailRec :: [Int] -> Int
myMinimumTailRec [n] = n
myMinimumTailRec (n:ns) = findMin n ns
    where
    findMin :: Int -> [Int] -> Int
    findMin candidate (n:ns)
        | n < candidate = findMin n ns
        | otherwise = findMin candidate ns
    findMin candidate [] = candidate

longest :: [String] -> String
longest [s] = s
longest (s:ss)
    | length s > length longestAfterFirst = s
    | otherwise = longestAfterFirst
    where
        longestAfterFirst :: String
        longestAfterFirst = longest ss

longestTailRec :: [String] -> String
longestTailRec [s] = s
longestTailRec (s:ss) = loop s ss
    where
    loop :: String -> [String] -> String
    loop candidate (s:ss)
        | lenFirst > lenCandidate = loop s ss
        | otherwise = loop candidate ss
        where
        lenCandidate = length candidate
        lenFirst = length s
    loop candidate [] = candidate

repeated :: [Char] -> [Char]
repeated [] = []
repeated (c:cs)
    | c `elem` cs = c : repeated [ c' | c' <- cs, c' /= c ]
    | otherwise = repeated cs

repeatedTailRec :: [Char] -> [Char]
repeatedTailRec list = reverse (collect [] list)
    where
    collect :: [Char] -> [Char] -> [Char]
    collect repeating [] = repeating
    collect repeating (c:cs)
        | c `elem` cs && c `notElem` repeating = collect (c : repeating) cs
        |    otherwise = collect repeating cs

everySecond :: [Char] -> [Char]
everySecond (_:c2:cs) = c2 : everySecond cs
everySecond _ = []

pairGreater :: [Int] -> [(Int, [Int])]
pairGreater list = pair list list
    where
    pair :: [Int] -> [Int] -> [(Int, [Int])]
    pair l [] = []
    pair l (n:ns) = (n, [ a | a <- l, a > n ]) : pair l ns


