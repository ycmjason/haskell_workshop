evenAddOneOddMinusOne :: Int -> Int
evenAddOneOddMinusOne x = if (x `mod` 2 == 0) then x+1 else x-1

absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

myTake :: Int -> [Int] -> [Int]
myTake 0 xs = []
myTake n [] = []
myTake n (x:xs) = x:(myTake (n-1) xs)
