Types and Type Classes
======================

You can examine the type of an expression in GHCi with `:t`.

`head :: [a] -> a`. `a` is a *type variable* (types start always with a capital letter). Functions that have type variables are called *polymorphic functions*.

To call an infix function in a prefix form, we need to surround it with parenthesis.

So a common pattern is to declare simple functions from the usual two parameters functions: (+3) is a function that takes a parameter and sums 3 to it (-> curried + function).

Let's examine: `(==) :: (Eq a) => a -> a -> Bool`
Everything before the => symbol is called a *class constraint*.
We can read the previous type declaration like this: the equality function takes any two values that are of the same type and returns a Bool. The type of those two values must be a member of the Eq class (this was the class constraint).
