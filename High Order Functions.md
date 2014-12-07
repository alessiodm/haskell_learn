High Order Functions
======================

Every function in Haskell officially only takes one parameter. All the functions that accepted several parameters so far have been curried functions.

Putting a space between two things is simply *function application*. The space is sort of like an operator and it has the highest precedence.

The following declarations are equivalent:
`max :: (Ord a) => a -> a -> a`
`max :: (Ord a) => a -> (a -> a)`

So how is that beneficial to us? Simply speaking, if we call a function with too few parameters, we get back a partially applied function.

Other equivalent declarations:
`-- compareWithHundred :: (Num a, Ord a) => a -> Ordering`
`compareWithHundred x = compare 100 x`
`compareWithHundred = compare 100`

The reduction is:
a -> b -> c -> d
equivalent to:
a -> (b -> (c -> d))
and the currying returns in order:
b -> (c -> d), c -> d

## Sections ##
Weird invocations on implicit left side parameter:

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])

divideByTen 200 == 200 / 10 == (/10) 200

The only special thing about sections is using -. From the definition of sections, (-4) would result in a function that takes a number and subtracts 4 from it. However, for convenience, (-4) means minus four. So if you want to make a function that subtracts 4 from the number it gets as a parameter, partially apply the subtract function like so: (subtract 4).

The -> is naturally right-associative

