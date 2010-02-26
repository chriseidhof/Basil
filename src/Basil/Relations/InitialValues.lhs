%if False

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls #-}
> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> module Basil.Relations.InitialValues where
>
> import Basil.Core
> import Basil.References
> import Data.HList


When we create a new entity in our ER model, we want to make sure that 
the initial relationships are also set. For a to-many relationship, we 
don't need any initial values, but for a to-one relationship it is essential.
This module calculates what those initial relationships are.

%endif

First, we introduce the notion of direction in a relationship. Consider the
relationship set |Rel phi One User Many Comment|. We can derive two functions
from this relationship set: the first function maps a |User| to many
|Comment|s. The other function maps a |Comment| to exactly one |User|. We
distinguish between these two functions with the types |L| and |R|, respectively.

> data L
> data R

Given a relationship and a direction we can compute both the source and target
(or: domain and codomain) of such a function:

> class    SourceType dir rel t | dir rel -> t
> instance SourceType L (Rel es c1 c2 l r) l
> instance SourceType R (Rel es c1 c2 l r) r

> class    TargetType dir rel t | dir rel -> t
> instance TargetType L (Rel es c1 c2 l r) r
> instance TargetType R (Rel es c1 c2 l r) l

When we create a new entity we want to store the initial relationships. We 
find those initial relationships by 
building a filter function on the type-level. We will filter out all the 
to-one relationship sets that apply to the given entity type. For example,
\todo.

The given entity type can be on either side of the relationship set, so we split 
up our function into two parts. |InitialValues| will look for |r| on the left-hand 
side of the relationship set, while |InitialValues'| will look for |r| on the
right-hand side of the relationship set.

> class HList ls => InitialValues  t ls ls' where
> class HList ls => InitialValues' t ls ls' where


> instance InitialValues t HNil HNil

> instance ( InitialValues' t (HCons rel xs) ls'
>          , TypeEq t  x isType
>          , TypeEq c2 One isOne
>          , HOr isType isOne shouldAppend
>          , rel ~ (Rel es c1 c2 from to)
>          , AppendIfTrue cond (InitialValue t dir rel) ls' result
>          ) =>
>   InitialValues rel (HCons rel xs) result


> instance ( InitialValues t xs ls'
>          , TypeEq t  x isType
>          , TypeEq c2 One isOne
>          , HOr isType isOne shouldAppend
>          , rel ~ (Rel es c1 c2 from to)
>          , AppendIfTrue cond rel ls' result
>          ) =>
>   InitialValues' rel (HCons rel xs) result


> class    AppendIfTrue cond   x xs xs' | cond x xs -> xs'
> instance AppendIfTrue HTrue  x xs (HCons x xs)
> instance AppendIfTrue HFalse x xs xs

> data InitialValue t dir rel = forall rels . InitialValue 
>    { ref   :: forall es . Ref es t,
>      relIx :: HLookupByHNat ixR entities rel => ixR
>    }
