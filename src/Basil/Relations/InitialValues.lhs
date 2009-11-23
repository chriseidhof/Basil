> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls #-}
> module Basil.Relations.InitialValues where
>
> import Basil.Cache
> import Basil.Core
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Relations.Base
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe

When we create a new entity in our ER model, we want to make sure that 
the initial relationships are also set. For a to-many relationship, we 
don't need any initial values, but for a to-one relationship it is essential.
This module calculates what those initial relationships are.

First, we introduce the notion of direction in a relationship. Consider the
relationship set |Rel phi One User Many Comment|. We can derive two functions from
this relationship set: the first function a |User| to many |Comment|s. The other
functions maps a |Comment| to exactly one |User|. We distinguish between these
functions with the types |L| and |R|, respectively.

> data L
> data R
> data Dir d where
>   DL :: Dir L
>   DR :: Dir R

Given a relationship and a direction we can compute the codomain or target of such a function:

> type family SourceType dir rel :: *
> type instance SourceType L (Rel phi c1 t1 c2 t2) = t1
> type instance SourceType R (Rel phi c1 t1 c2 t2) = t2

> type family TargetType dir rel :: *
> type instance TargetType L (Rel phi c1 t1 c2 t2) = t2
> type instance TargetType R (Rel phi c1 t1 c2 t2) = t1

When we create a new entity we want to store TODO.

> type InitialValue' phi r dir rel rels = (Ref phi (SourceType dir rel), InitialValue phi r dir rel rels)
> type InitialValue phi r dir rel rels = (Ref phi (TargetType dir rel), Dir dir, TIndex phi rel rels) 
> -- type InitialValue phi r dir rel rels = (TargetRef dir rel r, phi r, Dir dir, TIndex phi rel rels)


Therefore, we build a filter on the type-level. We will filter out all the 
to-one relationship sets that apply to the given entity type. The 
given entity type can be on either side of the relationship set, so we split 
up our function into two parts. |InitialValues| will look for |r| on the left-hand 
side of the relationship set, while |InitialValues'| will look for |r| on the
right-hand side of the relationship set. The |originalRels| is needed so we can 
create pointers into the original list of all relationship sets.

> type family   InitialValues  (phi :: * -> *) r rels originalRels :: *
> type family   InitialValues' (phi :: * -> *) r rels originalRels :: *

The base case is the empty list of relationship sets:

> type instance InitialValues phi r ()  f = ()

When we find a to-many relationship we will call |InitialValues'| to see if the other
direction of the relationship matches.

> type instance InitialValues phi r (Rel phi c1 from Many to, xs) f = 
>   InitialValues' phi r (Rel phi c1 from Many to, xs) f

However, when we find a to-one relationship we will include it in our |InitialValues| if 
the type is equal, using the type-level function |TypeEq|. We will also encode the 
direction |L| in which it was found.

> type instance InitialValues phi r (Rel phi c1 from One  to, xs) f = 
>   AppendIfTrue (TypeEq r from) 
>                (InitialValue phi r L (Rel phi c1 from One to) f) 
>                (InitialValues'   phi r   (Rel phi c1 from One to, xs) f)

The |InitialValues'| function is very similar, it looks in a different direction.

> type instance InitialValues' phi r (Rel phi One from c1  to, xs) f = 
>   AppendIfTrue (TypeEq r to)  
>                (InitialValue phi r R (Rel phi One from c1 to) f) 
>                (InitialValues phi r xs f)
> 
> type instance InitialValues' phi r (Rel phi Many from c1 to, xs) f = 
>   InitialValues phi r xs f
