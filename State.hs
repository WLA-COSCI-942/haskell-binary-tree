import Control.Applicative
import System.Random
import Control.Exception.Base

rollDiceIO :: IO (Int, Int) 
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

{-
Implement a function rollNDiceIO :: Int -> IO [Int] that, 
given an integer (a number of die rolls), 
returns a list of that number of pseudo-random integers 
between 1 and 6.
-}


rollNDiceIO :: Int -> IO [Int]
rollNDiceIO 0 = evaluate []
rollNDiceIO n = liftA2 (++) gen recursive
    where
        gen :: IO [Int]
        gen = invert $ [randomRIO (1,6)]

        recursive :: IO [Int]
        recursive = rollNDiceIO (n - 1)

        --IO Int 


invert :: [IO Int] -> IO [Int]
invert [] = evaluate []
invert ioList = evaluate $ [(head ioList)] 

