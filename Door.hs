data Door = Open | Close
    deriving (Show)

knock :: Door -> Door
knock _ = Open

close :: Door -> Door
close _ = Close