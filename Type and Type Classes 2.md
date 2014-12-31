Making our own types and type classes
======================================

```
ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967}
```

When making a new car, we don't have to necessarily put the fields in the proper order, as long as we list all of them. But if we don't use record syntax, we have to specify them in order.

Use record syntax when a constructor has several fields and it's not obvious which field is which.

A value constructor can take some values parameters and then produce a new value. For instance, the Car constructor takes three values and produces a car value. In a similar manner, type constructors can take types as parameters to produce new types.

```
data Maybe a = Nothing | Just a
```

The a here is the type parameter. And because there's a type parameter involved, we call Maybe a type constructor. Depending on what we want this data type to hold when it's not Nothing, this type constructor can end up producing a type of Maybe Int, Maybe Car, Maybe String, etc. No value can have a type of just Maybe, because that's not a type per se, it's a type constructor. In order for this to be a real type that a value can be part of, it has to have all its type parameters filled up.

Notice that the type of Nothing is Maybe a. Its type is polymorphic. If some function requires a Maybe Int as a parameter, we can give it a Nothing, because a Nothing doesn't contain a value anyway and so it doesn't matter. The Maybe a type can act like a Maybe Int if it has to, just like 5 can act like an Int or a Double. Similarly, the type of the empty list is [a]. An empty list can act like a list of anything. That's why we can do [1,2,3] ++ [] and ["ha","ha","ha"] ++ [].



```
data (Ord k) => Map k v = ...
```

However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations. Why? Well, because we don't benefit a lot, but we end up writing more class constraints, even when we don't need them.


## Derived Instances ##

We explained that a typeclass is a sort of an interface that defines some behavior. A type can be made an instance of a typeclass if it supports that behavior. Example: the Int type is an instance of the Eq typeclass because the Eq typeclass defines behavior for stuff that can be equated. And because integers can be equated, Int is a part of the Eq typeclass.


## Type Synonims ##
Previously, we mentioned that when writing types, the [Char] and String types are equivalent and interchangeable. That's implemented with type synonyms. Type synonyms don't really do anything per se, they're just about giving some types different names so that they make more sense to someone reading our code and documentation.

```
type String = [Char]
```

Just like we can partially apply functions to get new functions, we can partially apply type parameters and get new type constructors from them. Just like we call a function with too few parameters to get back a new function, we can specify a type constructor with too few type parameters and get back a partially applied type constructor.

```
type IntMap v = Map Int v
```

Type synonyms (and types generally) can only be used in the type portion of Haskell. We're in Haskell's type portion whenever we're defining new types (so in data and type declarations) or when we're located after a ::. The :: is in type declarations or in type annotations.


## Fixity ##

First off, we notice a new syntactic construct, the fixity declarations. When we define functions as operators, we can use that to give them a fixity (but we don't have to). A fixity states how tightly the operator binds and whether it's left-associative or right-associative. For instance, *'s fixity is:
 infixl 7 *
and +'s fixity is:
 infixl 6 +
That means that they're both left-associative (4 * 3 * 2 is (4 * 3) * 2) but * binds tighter than +, because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3.


Notice how we pattern matched on (x :-: xs). That works because pattern matching is actually about matching constructors. We can match on :-: because it is a constructor for our own list type and we can also match on : because it is a constructor for the built-in list type. Same goes for []. Because pattern matching works (only) on constructors, we can match for stuff like that, normal prefix constructors or stuff like 8 or 'a', which are basically constructors for the numeric and character types, respectively.


## Typeclasses 102 ##

A quick recap on typeclasses: typeclasses are like interfaces. A typeclass defines some behavior (like comparing for equality, comparing for ordering, enumeration) and then types that can behave in that way are made instances of that typeclass.

```
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)
```

```
data TrafficLight = Red | Yellow | Green
```

```
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False
```

Because == was defined in terms of /= and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration. That's called the minimal complete definition for the typeclass — the minimum of functions that we have to implement so that our type can behave like the class advertises. To fulfill the minimal complete definition for Eq, we have to overwrite either one of == or /=.

You can also make typeclasses that are subclasses of other typeclasses.


```
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False
```

With this instance declaration, we say this: we want all types of the form Maybe m to be part of the Eq typeclass, but only those types where the m (so what's contained inside the Maybe) is also a part of Eq. This is actually how Haskell would derive the instance too.


### Functor type class

```
class Functor f where  
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where  
    fmap = map

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

Again, notice how we wrote instance Functor Maybe where instead of instance Functor (Maybe m) where, like we did when we were dealing with Maybe and YesNo. Functor wants a type constructor that takes one type and not a concrete type.


## Types zen

Types are little labels that values carry so that we can reason about the values. But types have their own little labels, called kinds. A kind is more or less the type of a type.

