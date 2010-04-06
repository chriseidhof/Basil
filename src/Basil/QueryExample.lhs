%if False

> {-# LANGUAGE GADTs #-}
> module Basil.QueryExample where
>
> import Basil.Query

%endif

As an example, consider the |Person| datatype:

> data Person = Person {name :: String, age :: Int}
> chris = Person "chris" 25

We provide expression-level attributes (which can be mechanically derived using Template Haskell):

> name_  = Attribute "name"  name
> age_   = Attribute "age"   age

We now construct an expression that works on the person datatype:

> belowDrinkingAge :: Expr Person Bool
> belowDrinkingAge = age_ .<. Constant 18

Expressions are first-class: we can combine the |belowDrinking| age with other expressions:

> isChris :: Expr Person Bool
> isChris  =     Not (belowDrinkingAge) 
>          .&&.  name_ .==. Constant "chris"

And by applying the |eval| function to the |isChris| expression we get a function of type |Person -> Bool|.
However, evaluating |toSql isChris| yields an SQL expression:

\begin{spec}
"(NOT (age < 18)) AND (name == \"chris\")"
\end{spec}
