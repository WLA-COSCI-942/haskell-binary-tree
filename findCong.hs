listy :: [Int]
listy = take 1 (filter (\x -> (x `mod` 39) == 87) [1000..])