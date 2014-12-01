lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' a b = (fst a + fst b, snd a + snd b)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a + c, b + d)

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z

-- Pattern matching in list comprehension
-- let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
-- [ a + b | (a, b) <- xs ]

-- head function
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (h:_) = h

len :: (Num b) => [a] -> b
len [] = 0
len (h:t) = 1 + len t

-- Named pattern
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
-- max' a b | a > b = a | otherwise = b

-- Defining functions with backticks
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT

-- WHERE in guards!
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
--where bmi = weight / height ^ 2  
--    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials name surname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = name
          (l:_) = surname

-- You can define functions in the where block
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2

-- let
shoot = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

-- case expressions
head'' :: [a] -> a  
head'' xs = case xs of [] -> error "No head for empty lists!"  
                       (x:_) -> x

