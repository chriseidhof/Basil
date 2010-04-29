%if False

> {-# LANGUAGE GADTs #-}
> module Basil.Query where
> import Prelude hiding (LT)
> 

%endif

To represent queries, we introduces a new datatype |Expr| that describes the abstract syntax tree of a query.
This gives us full flexiblity to compile queries to a variety of target database systems.
The |Expr| datatype is parameterized over two types: the first type argument denotes the entity type of the query, and the second query datatype denotes the result of the query:

> data Expr entity a where
>   Attribute  :: String -> (entity -> att)                       -> Expr entity att
>   Constant   :: Show c => c                                     -> Expr entity c
>   Equal      :: Eq att   => Expr entity att -> Expr entity att  -> Expr entity Bool
>   LT         :: Ord att  => Expr entity att -> Expr entity att  -> Expr entity Bool
>   And        :: Expr entity Bool -> Expr entity Bool            -> Expr entity Bool
>   Not        :: Expr entity Bool                                -> Expr entity Bool

Given a value of |Expr entity a| we can construct a function |entity -> a| that can be used for querying the in-memory database
The function |eval| converts an |Expr| value into such a function:

> eval :: Expr entity a -> (entity -> a)
> eval (Attribute nm f)  e = f e
> eval (Constant  c)     e = c
> eval (Equal l r)       e = eval l e  ==  eval r e
> eval (LT    l r)       e = eval l e  <   eval r e
> eval (And   l r)       e = eval l e  &&  eval r e
> eval (Not   l)         e = not (eval l e)

Instead of building a function, we can also construct an |SQL| expression from a query. By using the |Expr| datatype for building queries, we guarantee that all constructed |SQL| expressions are well-formed and well-defined with respect to the table schema:

> toSql :: Expr entity a -> String
> toSql (Attribute nm f)  = nm
> toSql (Constant  c)     = show c
> toSql (Equal  l r)      = toSql l  ++ " == "  ++ toSql r
> toSql (LT     l r)      = toSql l  ++ " < "   ++ toSql r
> toSql (And    l r)      = parens (toSql l) ++ " AND " ++ parens (toSql r)
> toSql (Not l)           = "NOT " ++ parens (toSql l)

%if False

> parens x = "(" ++ x ++ ")"

> infix 4 .==.
> infix 4 .<.
> infix 3 .&&.

%endif

Finally, we provide smart constructors to make the usage of our library easier:

> (.==.) :: Eq att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.==.)  = Equal

> (.<.) :: Ord att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.<.)  = LT

> (.&&.) :: Expr entity Bool -> Expr entity Bool -> Expr entity Bool
> (.&&.)  = And
 
We have now defined a simple query language that can be used to construct queries on entities.
We compile queries to both Haskell functions and |SQL| expressions.
We continue by constructing some example queries.
