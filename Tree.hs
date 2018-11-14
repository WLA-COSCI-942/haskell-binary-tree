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