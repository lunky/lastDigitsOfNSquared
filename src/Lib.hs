module Lib
    ( someFunc
      ,green
      ,green'
      ,testGreen
      ,len
      ,seriesGenerator
    ) where

import Data.List

testGreen :: Integer -> Bool
testGreen n = (n == endsWith)  
  where   endsWith = (n^2) `mod` (10^(len n))

green :: Int -> Integer
green n = head $ drop (n-1) $  green' n

green' :: Int -> [Integer]
green' n = take n (seriesGenerator)

seriesGenerator :: [Integer]
seriesGenerator = 1 : merge (next 6) (next 5)
  where  next n 
          | testGreen n == True  = n : next (magnitude n)
          | otherwise = next (inc n)
            where magnitude x = x + 10 ^ (len n)
                  inc x = x + 10 ^ (len n-1)

merge :: (Ord a) => [a] -> [a] -> [a]
merge (a:as) (b:bs)
  | a < b     = a : merge as (b:bs)
  | a > b     = b : merge (a:as) bs
  | otherwise = a : merge as bs      
  
--len:: (Integral a, Integral a1) => a1 -> a
--len n = floor(logBase 10 ( ( abs n ))) + 1
len :: Show a => a -> Int
len n = length $ show n

someFunc :: IO ()
someFunc = putStrLn "someFunc"
