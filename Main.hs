-- Sum Types
data Color = Red | Blue | Green
data Fruit = Orange | Apple | Banana
data Pet   = Dog | Cat 

-- Product Type
data Person = Person {color :: Color, fruit :: Fruit, pet :: Pet}

-- Using types to solve a problem
data Nucleotide = A | G | T | C 
  deriving (Show, Eq)

data Pair a = Pair a a
  deriving (Show, Eq)

-- A possible implementation of pair 
data Pair' a b = Pair' a b
  deriving (Show, Eq)

type DNA = [Pair Nucleotide]

compliment :: Nucleotide -> Nucleotide
compliment A = T
compliment T = A
compliment C = G
compliment G = C

gene :: [Nucleotide]
gene = [C, T, A]

strand :: [Nucleotide]
strand = [A, C, C, T, A, G, T, A, A, T]

dnaComp :: [Nucleotide] -> DNA
dnaComp []     = []
dnaComp (n:ns) = Pair n (compliment n) : dnaComp ns 

hasGene :: [Nucleotide] -> [Nucleotide] -> Bool
hasGene _ [] = False
hasGene [] _ = True
hasGene g strand@(n:ns)  = if g == take (length g) strand then True else hasGene g ns

hasSubList :: (Eq a) => [a] -> [a] -> Bool
hasSubList _ [] = False 
hasSubList [] _ = True
hasSubList sub list@(head:tail) = if sub == take (length sub) list then True else hasSubList sub list

-- Tree data type
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert value Leaf = Node Leaf value Leaf
insert value (Node rTree x lTree)
  | value < x  = Node (insert value rTree) x lTree
  | value >= x = Node rTree x (insert value lTree)  

listToTree :: (Eq a, Ord a) => [a] -> Tree a
listToTree []  = Leaf
listToTree [x] = insert x Leaf 
listToTree (x:xs) = insert x (listToTree xs)

treeToList :: (Eq a, Ord a) => Tree a -> [a]
treeToList Leaf = []
treeToList (Node lTree value rTree) = (treeToList lTree) ++ [value] ++ (treeToList rTree)
  
testTree :: Tree Int
testTree = listToTree [1,5,3,4,8,0]

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