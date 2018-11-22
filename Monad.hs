import Control.Monad

instance Functor (State s) where
    fmap = Control.Monad.liftM
  
instance Applicative (State s) where
    pure = return
    (<*>) = Control.Monad.ap