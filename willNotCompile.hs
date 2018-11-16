import Prelude hiding (length)

length :: [Char] -> Int
length [ ] = 0
length ( _ : tail ) = 1 + length tail

isLucky :: Char -> Bool
isLucky '7' = True
isLucky _   = False

-- Function composition
isLuckyWord :: [Char] -> Bool
isLuckyWord = isLucky . length
