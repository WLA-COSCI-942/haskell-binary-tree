data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node rTree d lTree)
  | x < d = Node (insert x rTree) d lTree
  | x >= d = Node rTree d (insert x lTree)
  
playTree :: Tree Int
playTree = Leaf

build :: (Eq a, Ord a) => [a] -> Tree a
build [x] = insert x Leaf 
build (x:xs) = insert x (build xs)

inOrder :: (Eq a, Ord a) => Tree a -> [a]
inOrder Leaf = []
inOrder (Node lTree d rTree) = (inOrder lTree) ++ [d] ++ (inOrder rTree)
  

testTree :: Tree Int
testTree = (build [1,5,3,4,8,0])