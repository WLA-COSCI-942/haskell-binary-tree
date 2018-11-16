import Data.Ratio (Ratio, (%), denominator, numerator)

-- Sum Types
data Food  = Apple | Burger | Pasta | Sushi | Steak -- Cardinality of 5
data Color = Blue | Green | Red | Yellow -- Cardinality of 4
data Pet   = Dog | Cat | Bird -- Cardinality of 3

data Nonsense = Food | Color -- Cardinality of 5 + 4 = 9

data Useless a = Useless a
data Maybe a = Just a | Nothing 

-- (\x -> x + 1) anonymous function, lambda expression

-- Product Type
data Person = Person {food :: Food, color :: Color, pet :: Pet}

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

magic' :: a -> b
magic' = undefined

addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = magic (+) xs ys  

mulLists :: [Int] -> [Int] -> [Int]
mulLists xs ys = magic (*) xs ys

allOddNums :: [Int]
allOddNums = [2 * n + 1 | n <- [0..]]

allEvenNums :: [Int]
allEvenNums = [0, 2..]
 
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

{-
def ef(fr)
  ans = []
  if fr >= 1
    return [[fr.to_i], Rational(0, 1)]  if fr.denominator == 1
    intfr = fr.to_i
    ans, fr = [intfr], fr - intfr
  end
  x, y = fr.numerator, fr.denominator
  while x != 1
    ans << Rational(1, (1/fr).ceil)
    fr = Rational(-y % x, y * (1/fr).ceil)
    x, y = fr.numerator, fr.denominator
  end
  ans << fr
end
 
for fr in [Rational(43, 48), Rational(5, 121), Rational(2014, 59)]
  puts '%s => %s' % [fr, ef(fr).join(' + ')]
end
 
lenmax = denommax = [0]
for b in 2..99
  for a in 1...b
    fr = Rational(a,b)
    e = ef(fr)
    elen, edenom = e.length, e[-1].denominator
    lenmax = [elen, fr] if elen > lenmax[0]
    denommax = [edenom, fr] if edenom > denommax[0]
  end
end
puts 'Term max is %s with %i terms' % [lenmax[1], lenmax[0]]
dstr = denommax[0].to_s
puts 'Denominator max is %s with %i digits' % [denommax[1], dstr.size], dstr
-}