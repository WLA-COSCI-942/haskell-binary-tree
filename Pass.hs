-- Lowkey useless, but it compiles
import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 
import Control.Monad.Trans

main = do 
  password <- runMaybeT getPassword -- from IO (Maybe a) to Maybe a
  case password of 
    Just p  -> do
                putStrLn "valid password!"
                putStrLn p
    Nothing -> putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String 
getPassword = do 
  password <- lift getLine 
  guard (isValid password)
  return password 

-- Rewrite with MonadIO

-- 1. lift puts getLine into MaybeT, so we get IO String to MaybeT IO String, which is IO (Maybe String).
-- 2. Then <- Takes the (Maybe String) out of the MaybeT so, password is Maybe String

-- MoandTrans kind sig
-- MoandTrans :: ((* -> *) -> * -> *) -> Constraint
-- A funtion that takes a function that takes a function, some value, and returns some value?? Probably not
-- <- in do blocks pulls a value out of the IO context