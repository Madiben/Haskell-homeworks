--Mahdi Bentaleb ELKBLN solutions for practice 6`
import Data.Char ( toUpper )

data Vehicle = Car String Int | Bicycle
    deriving Show

numberOfPeople :: Vehicle -> Int
numberOfPeople (Car a b) = b
numberOfPeople Bicycle = 1
--https://people.inf.elte.hu/poor_a/en/fl03en.pdf
--1
not' :: Bool -> Bool 
not' True = False
not' False = True 

and' :: Bool -> Bool -> Bool 
and' True True = True 
and' _ _ = False 
--2
or' :: Bool -> Bool -> Bool 
or' True _ = True 
or' _ True = True 
or' _ _ = False
--3 xor
xor :: Bool -> Bool -> Bool 
xor True False = True 
xor False True = True 
xor _ _ = False
--4
add2 :: Int -> Int -> (Int,Int)
add2 a b
    | a+b == 0 =(0,0)
    | a+b == 2 = (0,1)
    | otherwise = (1,0)

--5
paren :: Char  -> Char  -> Bool 
paren '(' ')' = True 
paren '[' ']' = True 
paren '{' '}' = True 
paren _ _ = False

--6
calc :: (Int,Char,Int)->Int
calc (a, '+',b) = a+b 
calc (a, '-',b) = a-b 
calc (a, '*',b) = a * b 
calc (a, '/',b) = a `div` b 

--7
isSpace :: Char -> Bool 
isSpace ' '= True 
isSpace _ =False 
--Define modulo 3 multiplication. This means that for every x and y between 0 and 3, this expression will be true:
--http://lambda.inf.elte.hu/Patterns_en.xml#patterns-for-numbers-characters-and-strings
--x `mul3` y == (x * y) `mod` 3
--Use pattern matching to achieve this. Do not use the mod function or multiplication operator in your solution!
mul3 :: Int -> Int -> Int
mul3 0 _ = 0
mul3 _ 0 = 0
mul3 1 b = b
mul3 a 1 = a
mul3 2 2 = 1
--Define a function that replaces a line break with a space, and otherwise returns the same value. Recall that ‘’ is the character value for a newline.
replaceNewline :: Char -> Char
replaceNewline '\n' =' '
replaceNewline c = c 
--Define a function that replaces all line breaks with spaces. Use replaceNewline.
replaceNewlines :: String -> String
replaceNewlines a = [replaceNewline n|n<- a]
--Define a function that replaces the words “this” and “that” and vice versa.
swapThisAndThat :: String -> String
swapThisAndThat "that" = "this"
swapThisAndThat "this" = "that"
swapThisAndThat a = a
--Define a function that swaps all instances of “this” and “that”.
swapAllThisAndThat :: String -> String
swapAllThisAndThat a = unwords [swapThisAndThat n| n<- words a]
--List with pattern match
--Define a function that tests the given list is a singleton one. Use pattern matching.
--Match a list with exactly one item (“singleton”):
isSingleton :: [a] -> Bool
isSingleton [] = False 
isSingleton a = null (tail a)
--Exercise: Capitalize Initial Letter [*]
--Convert the initial letter of an arbitrary word to uppercase.
toUpperFirst :: String -> String
toUpperFirst ""=""
toUpperFirst (c:cs) = toUpper c : cs
--Exercise: Capitalize All Initial Letters
--Convert the initial letter of every word in a string to uppercase.
toUpperFirsts :: String -> String
toUpperFirsts a = unwords [toUpperFirst n| n<- words a]
--Patterns in List Comprehensions
--Patterns can be even employed in list comprehensions. Here is an intuitive example for this:
--[x | (x,1)<- [('c',2),('d',1),('e',1)]]

--Exercise: Identical Words in a Text
--Count the “a” words in a text.
countOfAs :: String -> Int
countOfAs []=0
countOfAs a = length [1|n <- words a, n=="a"] 

--Exercise: Filter by Distance of Elements
--Count pairs in a list where the distance between the components is greater than or equal to 2.

distantPairs :: [(Integer,Integer)] -> Int
distantPairs [] = 0
distantPairs (x:xs)
    | (snd x - fst x >= 2)|| distantPairs xs==1 = 1 
    | otherwise = 0


--Exercise: Every Fifth Element of a List
--Take every fifth element of a list.
everyFifth :: [a] -> [a]
everyFifth  a = [n|(n,b)<-zip a[0..],mod b 5==0]
