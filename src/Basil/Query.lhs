%if False

> {-# LANGUAGE GADTs #-}
> module Basil.Query where
> import Prelude hiding (LT)
> 

%endif

We will represent the queries above by adding a conditional expression to the
|find| method. The |find| method always works on one entity, so we want to limit
the conditional expression to that entity. As a first try, we could represent our |Expr| datatype like this:

> data Expr entity a where
>   Attribute  :: String -> (entity -> att)                       -> Expr entity att
>   Constant   :: Show c => c                                     -> Expr entity c
>   Equal      :: Eq att   => Expr entity att -> Expr entity att  -> Expr entity Bool
>   LT         :: Ord att  => Expr entity att -> Expr entity att  -> Expr entity Bool
>   And        :: Expr entity Bool -> Expr entity Bool            -> Expr entity Bool
>   Not        :: Expr entity Bool                                -> Expr entity Bool

Given a value of |Expr| we can calculate its value on an entity:

> eval :: Expr entity a -> entity -> a
> eval (Attribute nm f)  e = f e
> eval (Constant  c)     e = c
> eval (Equal l r)       e = eval l e  ==  eval r e
> eval (LT    l r)       e = eval l e  <   eval r e
> eval (And   l r)       e = eval l e  &&  eval r e
> eval (Not   l)         e = not (eval l e)

Alternatively, we can also build an SQL expression:

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

Before we show how these methods work, we will define some smart constructors:

> infix 4 .==.
> (.==.) :: Eq att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.==.)  = Equal

> infix 4 .<.
> (.<.) :: Ord att => Expr entity att -> Expr entity att -> Expr entity Bool
> (.<.)  = LT

> infix 3 .&&.
> (.&&.) :: Expr entity Bool -> Expr entity Bool -> Expr entity Bool
> (.&&.)  = And
 
