import Data.Ratio (Ratio, (%), denominator, numerator)
import Control.Applicative
import System.Random
import Control.Exception.Base

-- Sum Types
data Food  = Apple | Burger | Pasta | Sushi | Steak -- Cardinality of 5
data Color = Blue | Green | Red -- Cardinality of 3

data Nonsense = Food | Color -- Cardinality of 5 + 3 = 9

{-
map :: (a -> b) -> [a] -> [b] 
map _ [] = [] 
map f (x:xs) = f x : map f xs
-}



-- (\x -> x + 1) anonymous function, lambda expression

-- Product Type
data Person = Person {food :: Food, color :: Color}
-- data Person = Person Food Color

{-
We write A^B as the set of all functions f:B→A. 
Namely f is a function whose domain is B and takes values in A.
-}

(##) :: Int -> Int -> Bool
(##) = \x y -> x `mod` y == 0

c = 25 ## 5

funcMsg :: (Eq a, Show b) => (a -> a -> Bool) -> a -> a -> b -> b -> String
funcMsg f x y pass fail = if f x y then show pass else show fail

data Answer = Yep | Nope 
  deriving (Show)

isEqual :: Int -> Int -> String
isEqual x y = funcMsg (==) x y Yep Nope

divides :: Int -> Int -> String
divides x y = funcMsg (##) x y Yep Nope

hasSubList :: (Eq a) => [a] -> [a] -> Bool
hasSubList _ [] = False 
hasSubList [] _ = True
hasSubList sub list@(head:tail) = if sub == take (length sub) list then True else hasSubList sub list

-- Naive implementation
-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n + 1)

fibs :: [Int]
fibs = 0 : 1 : magic (+) fibs (tail fibs)

magic :: (a -> b -> c) -> [a] -> [b] -> [c]
magic f [] _  = []
magic f _  [] = []
magic f (a:as) (b:bs) = (f a b) : magic f as bs

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (a:as) = (f a) : map' f as 

addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = magic (+) xs ys  

mulLists :: [Int] -> [Int] -> [Int]
mulLists xs ys = magic (*) xs ys

infiniteEvens :: [Integer]
infiniteEvens = [0, 2..]
 
egyptianFraction :: Integral a => Ratio a -> [Ratio a]
egyptianFraction n
  | n < 0 = map negate (egyptianFraction (-n))
  | n == 0 = []
  | x == 1 = [n]
  | x > y = (x `div` y % 1) : egyptianFraction (x `mod` y % y)
  | otherwise = (1 % r) : egyptianFraction ((-y) `mod` x % (y * r))
  where
    x = numerator n
    y = denominator n
    r = y `div` x + 1

addThree :: Int -> Int 
addThree x = x + 3

isLucky :: Int -> String
isLucky 7 = "This is lucky!"
isLucky _ = "This is not lucky..."

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (head:tail) = myReverse tail ++ [head]

infiniteOdds :: [Integer]
infiniteOdds = [ 2 * n + 1 | n <- [0..]]

{-
Fun side note. Function composition is the equivalent of a syllogism in logic:

All men are mortal. Socrates is a man. Therefore, Socrates is mortal.

A syllogism composes two material implications into one:

(Man => Mortal), (Socrates => Man), therefore (Socrates => Mortal)
Therefore...

(b -> c) -> (a -> b) -> (a -> c)
... which is the type of the . function.
-}

data Shape = Square Float | Triangle Float Float

area :: Shape -> Float
area (Square side)          = side * side
area (Triangle base height) = 0.5 * base * height 

data Pair a = Pair a a
    deriving (Show)

data Pair' a b = Pair' a b 
    deriving (Show)

pairIntChar :: Pair' Int Char
pairIntChar = Pair' 7 'A'

pairInt :: Pair Int
pairInt = Pair 7 100
