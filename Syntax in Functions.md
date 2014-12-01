Syntax in Functions
===================

## Pattern Matching ##

Order is important in order to reach the "stopping condition".

Patterns can be non-exhaustive, and in that case we get exceptions.

NOTE: If you want to bind to several variables (even if one of them is just _ and doesn't actually bind at all), we have to surround them in parentheses!!!

Use named patterns (to avoid redundancy) using `@`.


## Guards ##

Same as big blocks of if-else statements... but more elegant!


## Where ##

In the *where* block notice that all the names are aligned at a single column. If we don't align them nice and proper, Haskell gets confused because then it doesn't know they're all part of the same block.


## Let binding ##

Let bindings let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards. Just like any construct in Haskell that is used to bind values to names, let bindings can be used for pattern matching.

The form is let `<bindings> in <expression>`. The names that you define in the let part are accessible to the expression after the in part.

The difference between *let* and *where* is that let bindings are expressions themselves. *where* bindings are just syntactic constructs.

If we want to bind to several variables inline, we obviously can't align them at columns. That's why we can separate them with semicolons.

## Case expressions ##

case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...

Pattern matching on parameters in function definitions is actually JUST syntactic sugar for case expressions!

Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere.
