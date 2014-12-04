maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
--maximum' (h:t)
--    | h > tailmax = h
--    | otherwise = tailmax
--    where tailmax = maximum' t
maximum' (h:t) = max h (maximum' t)


replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 


reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]  
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h1:t1) (h2:t2) = (h1,h2):zip' t1 t2


elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (h:t) = smaller ++ [h] ++ greater
    where smaller = quicksort [ e | e <- t, e < h ]
          greater = quicksort [ e | e <- t, e >= h ]
--quicksort (x:xs) =   
--    let smallerSorted = quicksort [a | a <- xs, a <= x]  
--        biggerSorted = quicksort [a | a <- xs, a > x]  
--    in  smallerSorted ++ [x] ++ biggerSorted