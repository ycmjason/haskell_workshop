module Showcase where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
