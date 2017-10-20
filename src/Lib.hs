module Lib
    ( someFunc
      ,green
      ,green'
      ,test
      ,testGreen
      ,len
    ) where

import Data.List

--testGreen n = ( show n ) `isSuffixOf` ( show $ n^2 )
testGreen :: Integer -> Bool
{-testGreen n = (( show n ) `isSuffixOf` ( show $ n^2 ))-}
{-testGreen n = (n `mod` 100 == 25 || n `mod` 100 == 76 ) && ( show n ) `isSuffixOf` ( show $ n^2 )-}
{-testGreen n = (n `mod` 5 ==0 || n `mod` 6 == 0) && ( show n ) `isSuffixOf` ( show $ n^2 )-}
{-testGreen n = (n==1 || n==5 || n==6 || (lastTwo == 25 || lastTwo == 76)) && (( show n ) `isSuffixOf` ( show $ n^2 ))-}
testGreen 1 = True 
testGreen 5 = True
testGreen 6 = True
testGreen n = (show n) `isSuffixOf` (show $ n^2)
--testGreen n = (n == lastTwo')  -- && (show n) `isSuffixOf` (show $ n^2)
--  where   lastTwo' = (n^2) `mod` (10^lengthOfN)
--          lengthOfN = len n

--greenGen Integer = testGreen num 
test :: [Integer] -> [Integer]
test n = take 5 n

green :: Int -> Integer
green n = head $ drop (n-1) $  green' n

{-green' :: Int -> [Integer]-}
green' n = take n $ filter (\y -> testGreen y) [1..]--(seriesGenerator)

seriesGenerator = [1,5,6,25,76] ++ foldr (\y acc -> y+25:(y+76:acc) )  []  [100,200..] 

len:: (Integral a, Integral a1) => a1 -> a
len n = floor(logBase 10 ( fromIntegral ( abs n ))) + 1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
