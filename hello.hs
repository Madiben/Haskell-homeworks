{-secondsInDays 1 == 86400
secondsInDays 0 == 0
secondsInDays 2 == 172800
secondsInDays 10 == 864000
divisible 1 == False
divisible 3 == False
divisible 5 == False
divisible 21 == True
divisible 63 == True
divisible 105 == False
divisible (-21) == True-}
day= 24 * 60 * 60
secondInDays ::Int->Int
secondInDays days = day*days

divisible ::Int->Bool
divisible days
  |days 'mod' 3==0 && days 'mod' 7==0 && days 'mod' 5/=0 = True
  |otherwise = False