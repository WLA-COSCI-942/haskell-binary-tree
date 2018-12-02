{-# LANGUAGE RankNTypes #-}

main = print $ rankN (+1)

rankN :: (forall n. Num n => n -> n) -> (Int, Double)
rankN f = (f 1, f 1.0)