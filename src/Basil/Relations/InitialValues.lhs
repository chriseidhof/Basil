%if False

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls ,
>              TypeOperators #-}
> module Basil.Relations.InitialValues where
>
> import Basil.Core
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Basil.Data.TList4
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe


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
> data Dir d where
>   DL :: Dir L
>   DR :: Dir R

Given a relationship and a direction we can compute both the source and target
(or: domain and codomain) of such a function:

> type family SourceType dir rel :: *
> type instance SourceType L  (Rel phi c1 t1 c2 t2) = t1
> type instance SourceType R  (Rel phi c1 t1 c2 t2) = t2

> type family TargetType dir rel :: *
> type instance TargetType L  (Rel phi c1 t1 c2 t2) = t2
> type instance TargetType R  (Rel phi c1 t1 c2 t2) = t1

When we create a new entity we want to store the initial relationships. We 
find those initial relationships by 
building a filter function on the type-level. We will filter out all the 
to-one relationship sets that apply to the given entity type.
For example, in the case of creating a new |Release| entity, the only included
relationship has to be |releases|.

The  difficult part is that a
given entity type can be on either side of the relationship set. Therefore, we split 
up our function into two parts. |InitialValues| will look for |r| on the left-hand 
side of the relationship set, while |InitialValues'| will look for |r| on the
right-hand side of the relationship set. The |originalRels| is needed so we can 
create pointers into the original list of all relationship sets.

> type family   InitialValues   phi r rels originalRels :: *
> type family   InitialValues'  phi r rels originalRels :: *

The base case is the empty list of relationship sets:

> type instance InitialValues phi r Nil  o = Nil

When we find a to-many relationship we will call |InitialValues'| to see if the other
direction of the relationship matches.

> type instance InitialValues phi r (Rel phi c1 from Many to :*: xs) o = 
>   InitialValues' phi r (Rel phi c1 from Many to :*: xs) o

However, when we find a to-one relationship we will include it in our |InitialValues| if 
the types |r| and |from| are equal, using the type-level functions |AppendIfTrue| and  |TypeEq|.
We will also encode the direction |L| in which it was found.

> type instance InitialValues phi r (Rel phi c1 from One  to :*: xs) o = 
>   AppendIfTrue  (TypeEq r from) 
>                 (InitialValue    phi r L (Rel phi c1 from One to) o) 
>                 (InitialValues'  phi r   (Rel phi c1 from One to :*: xs) o)

The |InitialValues'| function is very similar, it looks for |r| in a different direction by comparing it with |to| instead of |from|:

> type instance InitialValues' phi r (Rel phi One from c1  to :*: xs) o = 
>   AppendIfTrue  (TypeEq r to)  
>                 (InitialValue phi r R (Rel phi One from c1 to) o) 
>                 (InitialValues phi r xs o)
> 
> type instance InitialValues' phi r (Rel phi Many from c1 to :*: xs) o = 
>   InitialValues phi r xs o

An |InitialValue| for a relationship contains a reference to the target entity,
the direction of the relationship and the index of the relationship set into all
relationship sets in the domain model. 
We carry the |Dir| argument around explicitly so that we can pattern-match on it in the next module.

> type InitialValue phi r dir rel rels =  (  Ref phi (TargetType dir rel)
>                                         ,  Dir dir
>                                         ,  Ix rels rel) 
