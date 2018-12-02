data Suit = Diamond | Club | Heart | Spade
    deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Rank } | Joker
    deriving (Show) 

instance Eq Card where 
    (==) Joker _ = True
    (==) _ Joker = True
    (==) c1 c2 = s1 == s2 && r1 == r2 
        where
            s1 = suit c1
            s2 = suit c2
            r1 = rank c1
            r2 = rank c2


