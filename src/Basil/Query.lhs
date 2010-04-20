%if False

> {-# LANGUAGE GADTs #-}
> module Basil.Query where
> import Prelude hiding (LT)
> 

%endif

To represent queries, we introduces a new datatype |Expr| that describes the abstract syntax tree of a query.
This gives is full flexiblity to compile queries to a variety of targets.
The first type-argument is used to indicate the entity type, and the second argument captures the result type of the query.

> data Expr entity a where
>   Attribute  :: String -> (entity -> att)                       -> Expr entity att
>   Constant   :: Show c => c                                     -> Expr entity c
>   Equal      :: Eq att   => Expr entity att -> Expr entity att  -> Expr entity Bool
>   LT         :: Ord att  => Expr entity att -> Expr entity att  -> Expr entity Bool
>   And        :: Expr entity Bool -> Expr entity Bool            -> Expr entity Bool
>   Not        :: Expr entity Bool                                -> Expr entity Bool

Given a value of |Expr| we can calculate its value on an entity with the |eval| function. 
We use this function when querying the in-memory database.

> eval :: Expr entity a -> (entity -> a)
> eval (Attribute nm f)  e = f e
> eval (Constant  c)     e = c
> eval (Equal l r)       e = eval l e  ==  eval r e
> eval (LT    l r)       e = eval l e  <   eval r e
> eval (And   l r)       e = eval l e  &&  eval r e
> eval (Not   l)         e = not (eval l e)

Alternatively, we can also build an SQL expression.
We use this when querying the relational database.

> toSql :: Expr entity a -> String
> toSql (Attribute nm f)  = nm
> toSql (Constant  c)     = show c
> toSql (Equal  l r)      = toSql l  ++ " == "  ++ toSql r
> toSql (LT     l r)      = toSql l  ++ " < "   ++ toSql r
> toSql (And    l r)      = parens (toSql l) ++ " AND " ++ parens (toSql r)
> toSql (Not l)           = "NOT " ++ parens (toSql l)

%if False

> parens x = "(" ++ x ++ ")"

%endif

Finally, to make the usage of our library easier, we define some operators that act as smart constructors:

> infix 4 .==.
> (.==.) :: Eq att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.==.)  = Equal

> infix 4 .<.
> (.<.) :: Ord att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.<.)  = LT

> infix 3 .&&.
> (.&&.) :: Expr entity Bool -> Expr entity Bool -> Expr entity Bool
> (.&&.)  = And
 
