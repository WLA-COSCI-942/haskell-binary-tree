{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import System.Random
import Control.Exception.Base

data Suit = Diamonds | Clubs | Hearts | Spades
    deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Card = Card { suit :: Suit, rank :: Rank } | Joker
    deriving (Show) 

type Deck = [Card]

data Hand = Hand Card Card Card Card Card
    deriving(Eq)

instance Enum Suit where
    fromEnum :: Suit -> Int
    fromEnum Diamonds = 1
    fromEnum Clubs    = fromEnum Diamonds + 1
    fromEnum Hearts   = fromEnum Clubs + 1
    fromEnum Spades   = fromEnum Hearts + 1

    toEnum :: Int -> Suit
    toEnum 1 = Diamonds
    toEnum 2 = Clubs
    toEnum 3 = Hearts
    toEnum 4 = Spades

instance Enum Rank where 
    fromEnum :: Rank -> Int
    fromEnum Two   = 1
    fromEnum Three = fromEnum Two + 1
    fromEnum Four  = fromEnum Three + 1
    fromEnum Five  = fromEnum Four + 1
    fromEnum Six   = fromEnum Five + 1
    fromEnum Seven = fromEnum Six + 1
    fromEnum Eight = fromEnum Seven + 1
    fromEnum Nine  = fromEnum Eight + 1
    fromEnum Ten   = fromEnum Nine + 1
    fromEnum Jack  = fromEnum Ten + 1
    fromEnum Queen = fromEnum Jack + 1
    fromEnum King  = fromEnum Queen + 1
    fromEnum Ace   = fromEnum King + 1

    toEnum :: Int -> Rank
    toEnum 1  = Two
    toEnum 2  = Three
    toEnum 3  = Four
    toEnum 4  = Five
    toEnum 5  = Six
    toEnum 6  = Seven
    toEnum 7  = Eight
    toEnum 8  = Nine
    toEnum 9  = Ten
    toEnum 10 = Jack
    toEnum 11 = Queen
    toEnum 12 = King
    toEnum 13 = Ace

instance Enum Card where
    toEnum x      = Joker
    fromEnum card = case card of (Joker) -> 0
                                 (Card {suit, rank}) -> (fromEnum suit) + (fromEnum rank)

instance Eq Card where 
    (==) Joker _ = True
    (==) _ Joker = True
    (==) c1 c2   = s1 == s2 && r1 == r2 
        where
            s1 = suit c1
            s2 = suit c2
            r1 = rank c1
            r2 = rank c2
    (/=) c1 c2 = not $ (==) c1 c2

    {-
randoInt :: IO Int
randoInt 
-}

randomRank :: IO Rank
randomRank = toEnum <$> randomRIO (1, 13)

randomSuit :: IO Suit
randomSuit = toEnum <$> randomRIO (1,4)
