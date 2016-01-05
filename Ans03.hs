module Ans03 where

import Ans02(isPrime)

generatePrimes :: Int -> [Int]
generatePrimes n = filter isPrime [1..n]

isSubStringOf :: String -> String -> Bool
isSubStringOf originalxs ys = isSubStringOf' originalxs ys
  where
    isSubStringOf' :: String -> String -> Bool
    isSubStringOf' [] _  = True
    isSubStringOf' _ [] = False
    isSubStringOf' (x:xs) (y:ys)
      | x == y = isSubStringOf' xs ys
      | otherwise = isSubStringOf' originalxs (ys)

find :: String -> [String] -> [String]
find name userDB = filter (isSubStringOf name) userDB

findAll :: String -> [[String]] -> [String]
findAll name userDBs = foldl1 (++) (map (find name) userDBs)
-- findAll name = (foldl1 (++)).(map (find name))
-- findAll name userDBs = foldl1 (++) ((map (filter (isSubStringOf name))) userDBs)
