%if False

> {-# LANGUAGE GADTs #-}
> module Basil.QueryExample where
>
> import Basil.Query

%endif

As an example, consider the |Person| datatype:

> data Person = Person {name :: String, age :: Int}
> chris = Person "chris" 24

We provide expression-level attributes (which can be mechanically derived):

> name_  = Attribute "name"  name
> age_   = Attribute "age"   age

We now construct an expression that works on the person datatype:

> belowDrinkingAge :: Expr Person Bool
> belowDrinkingAge = age_ .<. Constant 18

And we can also combine expressions:

> isChris :: Expr Person Bool
> isChris  =     Not (belowDrinkingAge) 
>          .&&.  name_ .==. Constant "chris"

Evaluating |toSql isChris| yields the following SQL expression:

\begin{spec}
"(NOT (age < 18)) AND (name == \"chris\")"
\end{spec}
