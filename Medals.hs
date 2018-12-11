data Medal = Gold | Silver | Bronze 

{-
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a
-}


-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
    
instance Eq Medal where 
    Gold == Gold     = True
    Silver == Silver = True 
    Bronze == Bronze = True
    _ == _           = False 


instance Ord Medal where 
    Bronze <= Silver = True
    Silver <= Gold   = True
    Bronze <= Gold   = True 
    _ <= _           = False 


{-  
The Ord class is used for totally ordered datatypes.

Instances of Ord can be derived for any user-defined datatype whose constituent types are in Ord. The declared order of the constructors in the data declaration determines the ordering in derived Ord instances. The Ordering datatype allows a single comparison to determine the precise ordering of two objects.

The Haskell Report defines no laws for Ord. However, <= is customarily expected to implement a non-strict partial order and have the following properties:

Transitivity
if x <= y && y <= z = True, then x <= z = True
Reflexivity
x <= x = True
Antisymmetry
if x <= y && y <= x = True, then x == y = True
-}