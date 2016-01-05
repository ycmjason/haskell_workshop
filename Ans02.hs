module Ans02 where

import Ans01(bigger)

len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len xs

contains :: Int -> [Int] -> Bool
contains _ [] = False
contains n (x:xs)
  | n == x    = True
  | otherwise = contains n xs
-- contains n (x:xs) = n == x || contains n xs

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = isPrime' 2
  where
    isPrime' :: Int -> Bool
    isPrime' n 
      | n > (x `div` 2) = True
      | x `divisibleBy` n = False
      | otherwise = isPrime' (n+1)
--    isPrime' n = (n > x `div` 2) || not (x `divisibleBy` n) || isPrime'(n+1)

nextPrime :: Int -> Int
nextPrime x
  | isPrime (x+1) = x+1
  | otherwise     = nextPrime (x+1)

biggest :: [Int] -> Int
biggest [x] = x
biggest (x:xs) = bigger x (biggest xs)
