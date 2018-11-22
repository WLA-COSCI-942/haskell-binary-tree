{-
Fun side note. Function composition is the equivalent of a syllogism in logic:

All men are mortal. Socrates is a man. Therefore, Socrates is mortal.

A syllogism composes two material implications into one:

(Man => Mortal), (Socrates => Man), therefore (Socrates => Mortal)
Therefore...

(b -> c) -> (a -> b) -> (a -> c)
... which is the type of the . function.
-}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:tail) = myReverse tail ++ [h]


isPalindrome :: String -> Bool
isPalindrome x = myReverse x == x    




--pipe2 :: Int -> String

--pipe3 :: 