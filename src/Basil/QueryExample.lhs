%if False

> {-# LANGUAGE GADTs #-}
> module Basil.QueryExample where
>
> import Basil.Query

%endif

Consider the |Person| datatype and a value of |Person|:

> data Person = Person {name :: String, age :: Int}
> chris = Person "chris" 25

Using Template Haskell, we can provide |Attr| values for each field of |Person| that can be used in queries.

> name_  = Attribute "name"  name
> age_   = Attribute "age"   age

We construct an expression that works on the |Person| datatype and has |Bool| as its result:

> belowDrinkingAge :: Expr Person Bool
> belowDrinkingAge = age_ .<. Constant 18

Expressions are first-class: we can combine the |belowDrinking| age with other expressions:

> isChris :: Expr Person Bool
> isChris  =     Not (belowDrinkingAge) 
>          .&&.  name_ .==. Constant "chris"

If we apply the |eval| function to |isChris| we get a function with type |Person -> Bool|, which we can use in our in-memory database.
Alternatively, if we apply the |toSql| function we get the following |SQL| expression:

\begin{spec}
"(NOT (age < 18)) AND (name == \"chris\")"
\end{spec}
