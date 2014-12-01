--removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z
