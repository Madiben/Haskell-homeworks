module Practice4 where

-- Conversions
--------------

-- We have several number types:
--   Int, Integer, Float, Double, ...

-- ceiling, floor, round :: (RealFrac a, Integral b) => a -> b
-- ceiling, floor, round :: Double -> Int
-- ceiling, floor, round :: Double -> Integer

-- fromIntegral :: (RealFrac a, Integral b) => a -> b

-- fromIntegral :: Int -> Double
-- fromIntegral :: Integer -> Double
-- fromIntegral :: Int -> Integer
-- fromIntegral :: Integer -> Int

-- Compute the length of a vector with int coordinates.
vecLen :: (Int, Int) -> Double
vecLen (x, y) = sqrt (fromIntegral (x^2 + y^2))

-- vecLen (x, y) == square root of x^2+y^2
-- vecLen (1, 2) == sqrt 5.0

--- Compute the average of a list of integers.
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- average [1, 2, 3, 4] == 2.5

-- More lists operations
------------------------

-- zip :: [a] -> [b] -> [(a, b)]
-- (e.g. zip :: [Int] -> [Char] -> [(Int, Char)])
-- zip [1, 2, 3] ['a', 'b', 'c'] == [(1, 'a'), (2, 'b'), (3, 'c')]

-- zip   [1,        2,        3]
--       ['a',      'b',      'c']
-- ==    [(1, 'a'), (2, 'b'), (3, 'c')]

-- Example:
filterEvenPos :: [Char] -> [Char]
filterEvenPos cs = [ c | (i, c) <- zip [0 .. length cs-1] cs, even i ]
-- filterEvenPos ['a', 'b', 'c', 'd', 'e'] == ['a', 'c', 'e']

-- zip [0 .. length cs-1] cs == zip [0..] cs

getNth :: Int -> [Char] -> Char
getNth n xs = 
  if 0 <= n && n < length xs 
  then head [ x | (i, x) <- zip [0..] xs, i == n ]
  else error "index out of range"
-- getNth n xs = xs !! n

-- Examples:
--   getNth 0 ['a', 'b', 'c'] == 'a'
--   getNth 1 ['a', 'b', 'c'] == 'b'
--   getNth 2 ['a', 'b', 'c'] == 'c'
--   getNth 3 ['a', 'b', 'c']  is undefined

-- splitting lists:
--  take :: Int -> [a] -> [a]
--  drop :: Int -> [a] -> [a]
-- Example: 
--   take 5 [1..10] = [1,2,3,4,5]
--   drop 5 [1..10] = [6,7,8,9,10]

getNth' :: Int -> [Int] -> Int
getNth' n xs = head (drop n xs)

-- Appending lists:
--  (++)   :: [a] -> [a] -> [a]
--  concat :: [[a]] -> [a]
-- Examples:
--   [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
--   concat [ [1, 2], [3], [4, 5, 6] ] == [1,2,3,4,5,6]

-- Define a function rotate1 that rotates a list 1 step to the left. 
-- Examples:
--  rotate1 []           == []
--  rotate1 [1, 2, 3, 4] == [2, 3, 4, 1]
--  rotate1 [4, 2, 3, 1] == [2, 3, 1, 4]
rotate1 :: [Int] -> [Int]
rotate1 [] = []
rotate1 xs = tail xs ++ [head xs]

-- rotate1 [2, 3, 4, 1] 
--   == tail [2, 3, 4, 1] ++ [head [2, 3, 4, 1]]
--   == [3, 4, 1] ++ [2]
--   == [3, 4, 1, 2]

-- tail xs   = drop 1 xs
-- [head xs] = take 1 xs

-- Define a function rotateN that rotates a list n steps to the left. 
-- Examples:
--   rotateN 2 [1, 2, 3, 4] == [3, 4, 1, 2]
--   rotateN 6 [1, 2, 3, 4] == [3, 4, 1, 2]
rotateN :: Int -> [Int] -> [Int]
rotateN n [] = []
rotateN n xs = drop n' xs ++ take n' xs
             where n' = n `mod` length xs

-- 
allSquares :: [Int]
allSquares = [ n * n | n <- [1..] ]

firstNSquares :: Int -> [Int]
firstNSquares n = take n allSquares

-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime p = null [ d | d <- [2..p-1], p `mod` d == 0 ]

allPrimes :: [Int]
allPrimes = [ p | p <- [2..], isPrime p ]

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n allPrimes

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at even and odd positions:
-- (You can assume that the length of the input list is even.)
--  Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos xs = concat rs
  where evenXs = [ c | (i, c) <- zip [0..] xs, even i ]   
        oddXs  = [ c | (i, c) <- zip [0..] xs, odd i ]
        rs     = [ [o,e] | (o,e) <- zip oddXs evenXs ]
        -- rs = [ [2, 1], [4, 3] ]

swapEvenOddPos' :: [Int] -> [Int]
swapEvenOddPos' xs =
  let evenXs = [ c | (i, c) <- zip [0..] xs, even i ]   
      oddXs  = [ c | (i, c) <- zip [0..] xs, odd i ]
      rs     = [ [o,e] | (o,e) <- zip oddXs evenXs ]
  in concat rs