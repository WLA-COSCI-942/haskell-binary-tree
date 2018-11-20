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
rollNDiceIO 0 = pure []
rollNDiceIO n = appendIOList getListRand (rollNDiceIO (n - 1))
--rollNDiceIO n = l

-- (>>=) :: IO a -> (a -> IO b) -> IO b

function1 :: [Int] -> IO [Int]
function1 [] = pure []
function1 (x:xs) = liftA2 (++) (pure [x]) (pure xs)

appendIOList :: IO [a] -> IO [a] -> IO [a]
appendIOList = liftA2 (++)

getListRand :: IO [Int]
getListRand = liftA2 (:) (ioDiceRoll) (pure [])
    where 
        ioDiceRoll :: IO Int
        ioDiceRoll = randomRIO (1,6)

generator :: StdGen
generator = mkStdGen 76214378

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
        where
        (n, g) = randomR (1,6) (mkStdGen 234248)
        (m, _) = randomR (1,6) g

{-
Implement a function 
rollDice :: StdGen -> ((Int, Int), StdGen) that, given a generator, 
returns a tuple with our random numbers as the first element and the last generator as the second.
-}

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice x = ((n, m), l)
    where
    (n, g) = randomR (1,6) x
    (m, l) = randomR (1,6) g




