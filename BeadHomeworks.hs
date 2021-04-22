--Mahdi Bentaleb ELKBLN solutions for bead homeworks`
import Data.Char ( toUpper,toLower )

--Define a new type Temperature, which will be used to store temperature measurements. Temperature is measured during daytime (Daytime) and night (Night)

--Determine whether a measurement happened during daytime or night.
--isDaytime (Daytime 15)
--isDaytime (Daytime 0)
--isDaytime (Daytime (-2))
--not (isDaytime (Night (-4)))
--not (isDaytime (Night 0))
--not (isDaytime (Night 2))
data Temperature = Daytime Int | Night Int
    deriving Show

isDaytime :: Temperature -> Bool
isDaytime (Daytime a) = True 
isDaytime (Night b) = False 
     
--Given a sequence of measurements every 2 hours, determine the lowest night and the highest daytime temperatures. If the sequence does not contain night temperature, it should yield Nothing. The same holds for daytime temperatures.
--
--extremes [Night (-5), Night (-6), Daytime 0, Daytime 3, Daytime 5, Daytime 1, Night (-7)] == (Just 5, Just (-7))
--extremes [Night 5, Night 0, Daytime 1, Daytime 10, Daytime 8, Daytime 5, Night 2]         == (Just 10, Just 0)
--extremes [Night 3, Night 0, Daytime 1, Daytime 10, Daytime 8, Daytime 15, Night 7]        == (Just 15, Just 0)
--extremes [Night 3, Night 0, Night 7, Night 8]          == (Nothing, Just 0)
--extremes [Daytime 2, Daytime 6, Daytime 10, Daytime 8] == (Just 10, Nothing)
--extremes [] == (Nothing, Nothing)

extremes :: [Temperature] -> (Maybe Int, Maybe Int)
extremes [] = (Nothing, Nothing )
extremes a 
    | null ([x | (Daytime x) <- a]) = (Nothing ,Just (minimum[z|(Night z)<-a]))
    | null ([x | (Night x) <- a]) = (Just (maximum[x|(Daytime x)<-a]),Nothing )
    | not (null ([x | (Daytime x) <- a])) && not (null ([x|(Night x)<-a])) =(Just (maximum[x|(Daytime x)<-a]),Just (minimum[z|(Night z)<-a]))


-- homework 2 
--1. Months
--Define a new type Month, which should have a data constructor for each of the 12 months in a year: Jan, Feb etc.
--
--2. Days In A Month
--Define a function numberOfDays that can tell the number of days in a given month.
--
--You may define the function with pattern matching or you can store months and number of days in a list of pairs.
--
--Number of days in Februrary depends on whether the year is a leap year. Therefore, numberOfDays also takes the year as input:
--numberOfDays 2017 Jan == 31
--numberOfDays 2017 Aug == 31
--numberOfDays 2017 Sep == 30
--map (numberOfDays 2018) [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec] == [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
--numberOfDays 2016 Feb == 29
--numberOfDays 1600 Feb == 29
--numberOfDays 1700 Feb == 28
--leap year devided by 4 or devided by 100 and 400

data Month= Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show,Eq)

type Year = Int

numberOfDays :: Year -> Month -> Int
numberOfDays y Feb
    | (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 ==0 =29
    | otherwise = 28
numberOfDays y m
    | m `elem` [Jan , Mar , May , Jul , Aug , Oct , Dec] =31
    | m `elem` [Apr , Jun , Sep , Nov] = 30
--3
--Capitalize the first letters of all words and convert everything else to small case.
--
--Function words (link) can help you in breaking up the string into a list of words.
--
--You may define auxiliary functions.
--
--titleCase :: String -> String
--titleCase "Haskell" == "Haskell"
--titleCase "haskeLl" == "Haskell"
--titleCase "a tRee With APPLES." == "A Tree With Apples."
--titleCase "a" == "A"
--titleCase "" == ""

titleCase :: String -> String
titleCase ""=""
titleCase m= unwords [toUpper c : [toLower x|x<-cx]| (c:cx)<- words m]


--4
-- Haskell places restriction on names of constants and functions (collectively they are called identifiers). They have to start with a lower case letter and can include letters and numbers. Also, they have to be different from reserved words.
-- 
-- Your task is to check that an identifier is valid, that is, it contains only allowed characters and it is not a reserved word.
-- 
-- First letter
-- Define a function isIdentifierStart which returns True only if its parameter is a lower case letter or an underscore.
-- 
-- Also specify the type of the function.
-- 
-- All test cases following should yield True:
--isIdentifierStart 'a'
--isIdentifierStart 'x'
--isIdentifierStart '_'
--not (isIdentifierStart 'A')
--not (isIdentifierStart 'P')
--not (isIdentifierStart '#')
--not (isIdentifierStart '5')

isIdentifierStart :: Char -> Bool 
isIdentifierStart c
    | c `elem` ['a'..'z'] || c=='_' = True 
    | otherwise = False 

--Rest of the letters
--Define a function isIdentifierPart which returns True only if its parameter is a lower or upper case letter, a decimal digit or an underscore.
--
--Also specify the type of the function.
--
--All test cases following should yield True:

isIdentifierPart :: Char -> Bool 
isIdentifierPart c 
    | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] || c=='_' = True 
    | otherwise = False 

--Reserved words
--Some words are not allowed as identifiers. These are reserved words. Some of them in Haskell: if, then, else, module, import.
--
--Define a function isReserved which checks whether a string is reserved. Here we are only concerned with the five reserved words above.
--
--Also specify the type of the function.
--
--All test cases following should yield True:

isReserved :: String ->Bool 
isReserved c 
    |c `elem` ["if","then", "else", "module", "import"] = True
    |otherwise = False 

--Valid identifiers
--Using the functions above, define a function which returns True only if its parameter is a valid identifier, that is, it starts with a lower case letter or an underscore, and contains lower and upper case letters, decimal digits and underscores. It cannot be a reserved word.
--
--Also specify the type of the function.
--
--All test cases following should yield True:
isValid :: String -> Bool 
isValid ""= False 
isValid (c:cx) =not (isReserved (c:cx)) &&  isIdentifierStart c && and [isIdentifierPart x|x <- cx] 

-- and[boolean] returns the conjunction of a Boolean list, the result can be True only for finite lists



--1. takeWord
--Define a takeWord function which takes a string and returns its first word. It should take each character at a time from the beginning to the end until the first space character. It should return the characters before the first space character.
--
--Also specify the type of the function.
--
--Here are a couple of test cases:
--
--takeWord ""           == ""
--takeWord " tree"      == ""
--takeWord "apple tree" == "apple"
--takeWord "appletree"  == "appletree"
--takeWord "Haskell is functional " == "Haskell"
--2. dropWord
--Define a dropWord function which is similar to the takeWord, only it drops the first word of its argument.
--
--Also specify the type of the function.
--
--Here are a couple of test cases:
--
--dropWord ""           == ""
--dropWord " tree"      == " tree"
--dropWord "apple tree" == " tree"
--dropWord "appletree"  == ""
--takeWord "Haskell is functional " == " is functional "
--3. wordsOf
--Last, define a wordsOf function, which is identical to the words function of the standard library. The function takes a string as argument and yields its words in a list.
--
--Give a recursive definition of wordsOf.
--
--Also specify the type of the function.
--
--Here are a couple of test cases:
--
--wordsOf ""                                             == []
--wordsOf "hello"                                        == ["hello"]
--wordsOf "hello  "                                      == ["hello"]
--wordsOf "apple tree"                                   == ["apple","tree"]
--wordsOf "   Recursive    functions   are  common in   Haskell.    "            == ["Recursive","functions","are","common","in","Haskell."]

takeWord :: String -> String 
takeWord "" = ""
takeWord (a:x)  
    | a == ' ' = ""
    |a /= ' ' = a : takeWord x

dropWord :: String -> String
dropWord "" = ""
dropWord c
    | not (null [x|(x,z)<-zip [0..] c,z==' ']) =drop (minimum[x|(x,z)<-zip [0..] c,z==' ']) c
    | null [x|(x,z)<-zip [0..] c,z==' '] =""

wordsOf :: String ->[String ]
wordsOf "" =[]
wordsOf (c:cs) 
    | c==' ' = wordsOf cs
    | c/=' ' = takeWord (c:cs) : wordsOf (dropWord (c:cs))

--    Time Shift
--A specific time can be represented as a pair of integers (hour, minute).
--
--Define a function shift that shifts a time with given minutes.
--
--It is recommended to use function mod to handle overturn at 24th hour and 60th minute.
--
--Also specify the type of the function.
--
--Here are a couple of test cases:
--shift (12, 30) 15                 == (12, 45)
--shift (22, 10) 30                 == (22, 40)
--shift (10,  5) 60                 == (11,  5)
--shift (12,  5) 90                 == (13, 35)
--shift (08, 30) 90                 == (10,  0)
--shift (23,  0) 59                 == (23, 59)
--shift (23,  0) 60                 == ( 0,  0)
--shift (22, 10) (2 * 24 * 60 + 5)  == (22, 15)
--shift (22, 10) (3 * 24 * 60 + 65) == (23, 15)

shift :: (Int, Int ) -> Int -> (Int, Int )
shift (h , m) a 
    | a + m < 60 = (h,a+m)
    | a + m >60 && h>0 && h <= 23 =((h + (a + m) `div` 60)`mod`24,(a + m) `mod` 60) 
    | a + m == 60 && h>0 && h <= 23 =((h + 1)`mod`24,0) 

--Earlier time
--Define a function isEarlier that checks whether a time is earlier than another.
--
--Also specify the type of the function.
--
--Here are a couple of test cases:

isEarlier :: (Int, Int ) -> (Int, Int ) ->Bool 
isEarlier (h1,m1) (h2,m2)
    | h2 > h1 = True 
    | h1 > h2 = False 
    | h1 == h2 && m2 > m1 = True 
    | otherwise = False 

--Events
--An event (such as a todo item, a meeting, an appointment), consists of three things: it has starting and ending time, and a description of the event (such as “soccer with friends”).
--
--Define a function createEvent which takes the parameters and creates a new event. The parameters:
--
--starting time
--duration in minutes
--description
--Also specify the type of the function.
--
--Here are a couple of test cases:

createEvent :: (Int, Int ) -> Int -> String -> ((Int, Int ),(Int, Int ),String )
createEvent time min event = (time,shift time min,event)