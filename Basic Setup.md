Basic Setup
===========

The `ghci` is the interactive Haskell interpreter.
In order to load functions, just create a file `functions.hs` and type `:l functions` in the prompt.


## Tips ##

 - To change the prompt prelude: `:set prompt "ghci> "`

 - `5 * -3` error, while `5 * (-3)` works.

 - The not equal operator is `/=`

 - `+`, `-`, `*`, `/`, `==`, `/=` and so on are all *infix* functions

 - Usually functions are in the prefix form. The invocations is simply indicating the function name and the parameters separated by spaces

 - *Function application* has the highest precedence. `succ (9 * 10)` returns 91, while `succ 9 * 10` returns 100

 - You need to use _backticks_ for applying 2-arity functions in an infix form

 - Functions CAN'T begin with a capital letter!

 - When a function doesn't take any parameters, we usually say it's a *definition* (or a *name*)

 - We can use `let` to define a name directly in GHCi


## Lists ##

Lists are homogeneous: they contain objects of the same data type.

Strings are synctatic sugar for just plain list of characters, therefore we can use list operations on them.

The `++` operator concats two lists together.

Putting something at the end of a list that's fifty million entries long with `++` is going to take a while. However, putting something at the beginning of a list using the `:` operator (also called the cons operator) is instantaneous.

`[]` is an empty list.

If you want to get an element out of a list by index, use `!!`, e.g.,: `"Steve Buscemi" !! 6` returns `B`.

The lists within a list can be of different lengths but they can't be of different types.

Lists can be compared if the stuff they contain can be compared.


## Ranges ##

To make a list containing all the natural numbers from 1 to 20, you just write `[1..20]`

To make a list with all the numbers from 20 to 1, you can't just do [20..1], you have to do [20,19..1].

All the odd numbers between 1 and 43: [1,3..43].

Let's examine how you would get the first 24 multiples of 13. Sure, you could do `[13,26..24*13]`. But there's a better way: `take 24 [13,26..]`.


## List Comprehension ##

Example: `[x*2 | x <- [1..10]]` returns [2,4,6,8,10,12,14,16,18,20]

If you add a predicate: [x*2 | x <- [1..10], x*2 >= 12]
Weeding out lists by predicates is also called filtering

Another example: `boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]`

It is possible to include multiple predicates: `[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]`

We can also draw from several lists: `[ x*y | x <- [2,5,10], y <- [8,10,11]]` returns `[16,20,22,40,50,55,80,100,110]`
(isn't it a cross-product?)

Length in list comprehension: `length' xs = sum [1 | _ <- xs]`

## Tuples ##

Fixed length and not all objects of the same type. Use parenthesis to declare them.

The type of the tuple contain the tuple arity: so the same list cannot contain tuples of different sizes.

You also couldn't make a list like [(1,2),("One",2)] because the first element of the list is a pair of numbers and the second element is a pair consisting of a string and a number.

