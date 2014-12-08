applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--zipWith' (+) [4,2,5,6] [2,6,2,3]
--zipWith' max [6,3,2,1] [7,3,1,5]
--zipWith'' = zipWith' (*)

--flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f = g
--    where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y

double a = a * 2

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = f h : map' f t

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p l = [ x | x <- l, p x == True ]

--q1 = head [ x | x <- [100000,99999..], mod x 3829 == 0]
q1 = head (filter p [100000,99999..])
     where p x = x `mod` 3829 == 0

q2 = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

--cltzOp x = if x `mod` 2 == 0 then x `div` 2 else x * 3 + 1
--cltz 1 = [1]
--cltz n = n : cltz x
--         where x = cltzOp n

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n * 3 + 1)

numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 
