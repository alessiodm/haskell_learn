
Monads
======================================

We'll learn about monads, which are just beefed up applicative functors, much like applicative functors are only beefed up functors.

Like we said, an applicative value can be seen as a value with an added context. A fancy value, to put it in technical terms. For instance, the character 'a' is just a normal character, whereas `Just 'a'` has some added context. Instead of a `Char`, we have a `Maybe Char`, which tells us that its value might be a character, but it could also be an absence of a character.

Monads are a natural extension of applicative functors and with them we're concerned with this: if you have a value with a context, `m a`, how do you apply to it a function that takes a normal `a` and returns a value with a context? That is, how do you apply a function of type `a -> m b` to a value of type `m a`? So essentially, we will want this function:

```
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

_If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?_ This is the main question that we will concern ourselves when dealing with monads. We write `m a` instead of `f a` because the `m` stands for Monad, but monads are just applicative functors that support >>=. The >>= function is pronounced as bind.

Let's call it `applyMaybe`:

```
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x 
```

And let's play with it:

```
ghci> Just 3 `applyMaybe` \x -> Just (x+1)  
Just 4  
ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  
Just "smile :)"  
ghci> Nothing `applyMaybe` \x -> Just (x+1)  
Nothing  
ghci> Nothing `applyMaybe` \x -> Just (x ++ " :)")  
Nothing
```

## Definition

```
class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg
```

The first function that the Monad type class defines is `return`. It's the same as `pure`, only with a different name. It takes a value and puts it in a minimal default context that still holds that value.

The next function is `>>=, or bind. It's like function application, only instead of taking a normal value and feeding it to a normal function, it takes a monadic value (that is, a value with a context) and feeds it to a function that takes a normal value but returns a monadic value.

Next up, we have `>>`. We won't pay too much attention to it for now because it comes with a default implementation and we pretty much never implement it when making Monad instances.

The final function of the Monad type class is `fail. We never use it explicitly in our code. Instead, it's used by Haskell to enable failure in a special syntactic construct for monads that we'll meet later. We don't need to concern ourselves with fail too much for now.

So `Maybe` instance of `Monad`:

```
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing
```

## Example

We can apply functions by first writing the parameter and then the function:

```
x -: f = f x
```

For example:

```
ghci> 100 -: (*3)  
300  
ghci> True -: not  
False  
ghci> (0,0) -: landLeft 2  
(2,0)
```

Now thinking about the monad example:

```
ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2) 

ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
Nothing
```

We couldn't have achieved this by just using `Maybe` as an applicative. If you try it, you'll get stuck, because applicative functors don't allow for the applicative values to interact with each other very much. They can, at best, be used as parameters to a function by using the applicative style. The applicative operators will fetch their results and feed them to the function in a manner appropriate for each applicative and then put the final applicative value together, but there isn't that much interaction going on between them. Here, however, each step relies on the previous one's result. On every landing, the possible result from the previous one is examined and the pole is checked for balance. This determines whether the landing will succeed or fail.


Instead of making functions that ignore their input and just return a predetermined monadic value, we can use the >> function, whose default implementation is this:

```
(>>) :: (Monad m) => m a -> m b -> m b  
m >> n = m >>= \_ -> n 

ghci> Nothing >> Just 3  
Nothing  
ghci> Just 3 >> Just 4  
Just 4  
ghci> Just 3 >> Nothing  
Nothing
```

If you replace >> with >>= \_ ->, it's easy to see why it acts like it does.


## DO notation

`do` notation isn't just for IO, but can be used for any monad. Its principle is still the same: gluing together monadic values in sequence.

To understand:

```
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))
```

To save us from writing all these annoying lambdas, Haskell gives us `do` notation. It allows us to write the previous piece of code like this:

```
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)
```

Because do expressions are written line by line, they may look like imperative code to some people. But the thing is, they're just sequential, as each value in each line relies on the result of the previous ones, along with their contexts (in this case, whether they succeeded or failed).

In do notation, when we bind monadic values to names, we can utilize pattern matching, just like in let expressions and function parameters.

When pattern matching fails in a do expression, the `fail` function is called. It's part of the Monad type class and it enables failed pattern matching to result in a failure in the context of the current monad instead of making our program crash.


### Curiosity

But wait, didn't we say that monads are just beefed up applicative functors? Shouldn't there be a class constraint in there along the lines of class (Applicative m) = > Monad m where so that a type has to be an applicative functor first before it can be made a monad? Well, there should, but when Haskell was made, it hadn't occured to people that applicative functors are a good fit for Haskell so they weren't in there. But rest assured, every monad is an applicative functor, even if the Monad class declaration doesn't say so.

