module Ans01 where
isFifteen :: Int -> Char
isFifteen 15 = 'T'
isFifteen _ = 'F'

-- isFifteen :: Int -> Char
-- isFifteen x
--   | x == 15   = 'T'
--   | otherwise = 'F'

bigger :: Int -> Int -> Int
bigger x y
  | x > y = x
  | otherwise = y

biggestOf3 :: Int -> Int -> Int -> Int
biggestOf3 x y z
  | bigger x y > z = bigger x y
  | otherwise = z

-- biggestOf3 x y z
--   | x > y && x > z = x
--   | y > x && y > z = y
--   | z > x && z > y = z
