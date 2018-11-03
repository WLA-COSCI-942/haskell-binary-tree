data Color = Red | Blue | Green
data Fruit = Orange | Apple | Banana
data Pet   = Dog | Cat 

data Maybe a = Just a | Nothing

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

data Person = Person {color :: Color, fruit :: Fruit, pet :: Pet}

listToTree :: (Eq a, Ord a) => [a] -> Tree a
listToTree []  = Leaf
listToTree [x] = insert x Leaf 
listToTree (x:xs) = insert x (listToTree xs)

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node rTree value lTree)
  | x < value  = Node (insert x rTree) value lTree
  | x >= value = Node rTree value (insert x lTree)

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

magic :: (a -> b -> c) -> [a] -> [b] -> [c]
magic _ [] [] = []
magic f [] _  = []
magic f _  [] = []
magic f (a:as) (b:bs) = (f a b) : magic f as bs

addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = magic (+) xs ys  

mulLists :: [Int] -> [Int] -> [Int]
mulLists xs ys = magic (*) xs ys