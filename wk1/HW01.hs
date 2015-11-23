{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0    = []
    | otherwise = lastDigit n:[] ++ toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [ if i `mod` 2 == 0 then x else x*2
                        | (i,x) <- zip [0..] xs]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum [ sum (toRevDigits x) | x <- xs ]


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = sumDigits (doubleEveryOther (toRevDigits n)) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= -1   = []
    | n == 1    = [(a, c)]
    | n == 2    = [(a, b), (a, c), (b, c)]
    | otherwise = hanoi (n - 1) a c b ++ [(a, c)] ++ hanoi (n - 1) b a c

findK :: Integer -> Integer
findK n = round (sqrt (fromIntegral (2*n + 1))) - 1

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
    | n <= -1   = []
    | n == 1    = [(a, d)]
    | n == 2    = [(a, b), (a, d), (b, d)]
    | n == 3    = [(a, b), (a, c), (a, d), (c, d), (b, d)]
--    | n == 4    = [(a, d), (a, c), (a, b), (d, c), (a, d),
--                   (b, d), (c, a), (c, d), (a, d)]
--    | n == 5    = hanoi' 3 a b d c ++ [(a, b), (a, d), (b, c)] ++ hanoi' 3 c a b d
    | otherwise = hanoi' (n - findK n) a b d c ++
                  hanoi (findK n) a b d ++
                  hanoi' (n - findK n) c a b d
