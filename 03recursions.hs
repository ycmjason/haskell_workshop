power :: Int -> Int -> Int
power x 0 = 1
power x n = x * (power x (n-1))
-- power x n 
--   | n==0 = 1
--   | otherwise = x * (power x (n-1))
numberOfDivider :: Int -> Int
numberOfDivider x = numberOfDivider' x
  where
    numberOfDivider' :: Int -> Int
    numberOfDivider' 0 = 1
    numberOfDivider' divider
      | x `mod` divider == 0 = 1 + numberOfDivider' (divider-1)
      | otherwise            = numberOfDivider' (divider-1)
