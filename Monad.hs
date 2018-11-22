
import Control.Monad.Trans.State

--import Control.Monad.State

{-
Reimplement these instances by using explicitly the State constructor 
and functions instead of relying on the Monad instance.
-}

--newtype State s a = State { runState :: s -> (a,s) }

--instance Functor (State s) where
--    fmap = undefined

{-
instance Functor (State s) where
    fmap = Control.Monad.liftM
  
instance Applicative (State s) where
    pure = return
    (<*>) = Control.Monad.ap
-}

-- Didn't know this was possible
addTwoIfTwo :: Int -> Int
addTwoIfTwo x | x == 2 = x + 2 | otherwise = x 