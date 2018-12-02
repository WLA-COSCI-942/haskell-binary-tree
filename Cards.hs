data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Rank } | Joker
    deriving (Show) 

type Deck = [Card]

data Hand = Hand Card Card Card Card Card
    deriving(Eq)

{-
instance Enum Hand where
    toEnum = undefined
    fromEnum = undefined
-}

instance Eq Card where 
    (==) Joker _ = True
    (==) _ Joker = True
    (==) c1 c2 = s1 == s2 && r1 == r2 
        where
            s1 = suit c1
            s2 = suit c2
            r1 = rank c1
            r2 = rank c2
    (/=) c1 c2 = not $ (==) c1 c2

