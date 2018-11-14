-- Sum Types
data Color = Red | Blue | Green --Cardinality of 3
data Fruit = Orange | Apple | Banana --Cardinality of 3
data Pet   = Dog | Cat --Cardinality of 2

-- (\x -> x + 1) anonymous function, lambda expression

-- Product Type
data Person = Person {color :: Color, fruit :: Fruit, pet :: Pet}

hasSubList :: (Eq a) => [a] -> [a] -> Bool
hasSubList _ [] = False 
hasSubList [] _ = True
hasSubList sub list@(head:tail) = if sub == take (length sub) list then True else hasSubList sub list

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' [x,y] = [y,x]
reverse' (x:xs) = reverse xs ++ [x]

-- Naive implementation
-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n + 1)

fibs :: [Int]
fibs = 0 : 1 : magic (+) fibs (tail fibs)

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (a:as) = (f a) : map' f as 

magic :: (a -> b -> c) -> [a] -> [b] -> [c]
magic _ [] [] = []
magic f [] _  = []
magic f _  [] = []
magic f (a:as) (b:bs) = (f a b) : magic f as bs

addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = magic (+) xs ys  

mulLists :: [Int] -> [Int] -> [Int]
mulLists xs ys = magic (*) xs ys

