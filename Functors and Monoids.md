
Functors and Monoids
======================================

Remembering the functor definition:

```
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Using functors for mapping IO data:

```
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)

-- THE FOLLOWING...
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- ...IS THE SAME AS:
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

```

Mapping one function over a function has to produce a function, just like mapping a function over a Maybe has to produce a Maybe and mapping a function over a list has to produce a list. Et voilà:

```
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))
```

What does the type ```fmap :: (a -> b) -> (r -> a) -> (r -> b)``` for this instance tell us? Function composition!
In fact, another way to write this instance would be:

```
instance Functor ((->) r) where  
    fmap = (.)
```

This makes the revelation that using fmap over functions is just composition sort of obvious.


## Functor Laws ##

These laws aren't enforced by Haskell automatically, so you have to test them out yourself.

The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor.

```fmap id = id``

The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one. To write it in another way, for any functor F, the following should hold:

```fmap (f . g) F = fmap f (fmap g F)```

Here is an example of NON obeying functor:

```
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)
```

It violates the first law:

```
ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"
```


## Applicative Functors ##

We see how by mapping "multi-parameter" functions over functors, we get functors that contain functions inside them.

```
ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]
``

Note that fmap 'instanciated' by the function type is function composition! That's why for the one above, we can map _functions that take these functions as parameters_ over them.

But what if we have a functor value of Just (3 *) and a functor value of Just 5 and we want to take out the function from Just (3 *) and map it over Just 5? With normal functors, we're out of luck, because all they support is just mapping normal functions over existing functors.

We need the ```Applicative``` typeclass:

```
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b
```

The implementation for Maybe is:

```
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
```

instance Applicative [] where
    pure x = [x]
    (fh|ft) <*> (h|t) = (fmap fh h | ft <*> t)

We can do something like:

```
ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing
```

This becomes even more handy and apparent if we consider the fact that pure f <*> x equals fmap f x. This is why Control.Applicative exports a function called <$>, which is just fmap as an infix operator. Here's how it's defined:

```
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x
```

```
ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"
ghci> (++) "johntra" "volta"  
"johntravolta"
```

Considering lists:

```
ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]
```

Note that we have all permutations!!! Remember the all possible products greater than 50?

```
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
[16,20,22,40,50,55,80,100,110]
ghci> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
[55,80,100,110]
```

Considering the IO type class:

```
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)
```

So this is an example of two equivalent IO operations:

```
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b
```

```
myAction :: IO String  
myAction = (++) <$> getLine <*> getLine 
```

Since the result is anyway an IO action, we can use a ```do``` easily:

```
main = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
```

When we do something like ```(+) <$> Just 3 <*> Just 5```, we're using + on values that might or might not be there, which also results in a value that might or might not be there. When we do ```(+) <$> (+10) <*> (+5)```, we're using + on the future return values of (+10) and (+5) and the result is also something that will produce a value only when called with a parameter.

### Applicative Functors Laws ###

We just list them without explaining all of them:

```
pure f <*> x = fmap f x
pure id <*> v = v
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
```


## The newtype Keyword

There are more ways for a list to be an applicative functor: one way is to have <*> take every function out of the list that is its left parameter and apply it to every value in the list that is on the right, the second way is to take the first function on the left side of <*> and apply it to the first value on the right, then take the second function from the list on the left side and apply it to the second value on the right, and so on. the ```ZipList a``` type was introduced for this reason. That's how the ZipList is defined:

```
newtype ZipList a = ZipList { getZipList :: [a] }
```

(using record syntax so that we automatically get a function that extracts a list from a ZipList)

Instead of the data keyword, the newtype keyword is used. Now why is that? Well for one, newtype is faster. If you use the data keyword to wrap a type, there's some overhead to all that wrapping and unwrapping when your program is running. But if you use newtype, Haskell knows that you're just using it to wrap an existing type into a new type (hence the name), because you want it to be the same internally but have a different type. With that in mind, Haskell can get rid of the wrapping and unwrapping once it resolves which value is of what type.

So why not just use newtype all the time instead of data then? Well, when you make a new type from an existing type by using the newtype keyword, you can only have one value constructor and that value constructor can only have one field. But with data, you can make data types that have several value constructors and each constructor can have zero or more fields.

The newtype keyword in Haskell is made exactly for these cases when we want to just take one type and wrap it in something to present it as another type.

We mentioned that newtype is usually faster than data. The only thing that can be done with newtype is turning an existing type into a new type, so internally, Haskell can represent the values of types defined with newtype just like the original ones, only it has to keep in mind that the their types are now distinct. This fact means that not only is newtype faster, it's also lazier.

Remembering what lazy means:

```
ghci> undefined  
*** Exception: Prelude.undefined  
ghci> head [3,4,5,undefined,2,undefined]  
3
```

This difference in behavior may seem trivial, but it's actually pretty important because it helps us realize that even though types defined with data and newtype behave similarly from the programmer's point of view because they both have value constructors and fields, they are actually two different mechanisms. Whereas data can be used to make your own types from scratch, newtype is for making a completely new type out of an existing type. Pattern matching on newtype values isn't like taking something out of a box (like it is with data), it's more about making a direct conversion from one type to another.


### Recap: type vs. newtype vs. data

The ```type``` keyword is for making type synonyms. What that means is that we just give another name to an already existing type so that the type is easier to refer to.

The ```newtype``` keyword is for taking existing types and wrapping them in new types, mostly so that it's easier to make them instances of certain type classes. When we use newtype to wrap an existing type, the type that we get is separate from the original type.

The ```data``` keyword is for making your own data types and with them, you can go hog wild. They can have as many constructors and fields as you wish and can be used to implement any algebraic data type by yourself.


## Monoids

A monoid is a function that:

 - Takes two parameters
 - The parameters and the returned value have the same type
 - There exists such a value that doesn't change other values when used with the binary function
 - Moreover, it has the associativity property (e.g., (3+4)+5 = 3+(4+5))

Note the monoids are not commutative.

In Haskell it is defined like that:

```
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty
```

Laws of monoids:

```
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

It's possible to make instances of Monoid that don't follow these rules, but such instances are of no use to anyone because when using the Monoid type class, we rely on its instances acting like monoids.

Here is an interesting use of the Ordering Monoid properties: comparing to strings, first with lenght then alphabetically:

```
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a
```

But since Ordering is a monoid:

```
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)
```

Very interesting is the `Foldable` example.