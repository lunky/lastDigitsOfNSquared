module Lib
    ( someFunc
      ,green
      ,green'
      ,test
      ,testGreen
      ,len
      ,seriesGenerator
    ) where

import Data.List

testGreen :: Integer -> Bool
testGreen 1 = True 
testGreen 5 = True
testGreen 6 = True
testGreen n = (show n) `isSuffixOf` (show $ n^2)

test :: [Integer] -> [Integer]
test n = take 5 n

green :: Int -> Integer
green n = head $ drop (n-1) $  green' n

green' :: Int -> [Integer]
green' n = take n (seriesGenerator)

seriesGenerator :: [Integer]
seriesGenerator = 1 : merge (next 6) (next 5)

next n 
  | testGreen n == True  = n : next (magnitude n)
  | otherwise = next (inc n)
  where magnitude x = x + 10 ^ (len n)
        inc x = x + 10 ^ (len n-1)

merge :: (Ord a) => [a] -> [a] -> [a]
merge (a:as) (b:bs)
  | a < b     = a : merge as (b:bs)
  | a > b     = b : merge (a:as) bs
  | otherwise = a : merge as bs      

    
len:: (Integral a, Integral a1) => a1 -> a
len n = floor(logBase 10 ( fromIntegral ( abs n ))) + 1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
