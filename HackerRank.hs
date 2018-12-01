import Data.Char

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter eureka [a..b]-- your code

eureka :: Int -> Bool
eureka x = x == (sum $ (raiseList . toDigits) x)

toDigits :: Int -> [Int]
toDigits x = map (digitToInt) (show x) 

raiseList :: [Int] -> [Int]
raiseList x = 