-- definition can be constant or function
--exemple 
n :: Int  -- constant type 
n = 42

{- multi line comment
g
g-}

s= "Haskall"

--some functions 
even' :: Int -> Bool
even' n = n `mod` 2 == 0 -- 'backtick

odd' :: Int -> Bool
odd' n = n `mod` 2 /= 0 -- '/= instead of !=

odd'' :: Int -> Bool
odd'' n = not (even' n) -- '