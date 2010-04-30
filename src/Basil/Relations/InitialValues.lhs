> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls ,
>              TypeOperators #-}
> module Basil.Relations.InitialValues where
>
> import Basil.Core
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import qualified Data.Set as S


When we create a new entity, we guarantee that all the corresponding
relationships are initialized. For example, in our compilers ER model, whenever
we create a new |Release| entity, we guarantee that a relationship between
|Compiler| and |Release| is added, because the relationship set
\relationship{releases} describes that every |Release| should be related to
a single |Compiler|. 

First, we introduce the notion of direction in a relationship. Consider the
relationship type |Rel entities One User Many Comment|. We can derive two functions
from this relationship type: the first function maps a |User| to many
|Comment|s. The other function maps a |Comment| to exactly one |User|. We
distinguish between these two functions with the types |L| and |R|, respectively.

> data L
> data R
> data Dir d where
>   DL :: Dir L
>   DR :: Dir R

Given a relationship and a direction we can compute both the source and target types
of such a derived function. Also, we can compute the target cardinality.
This is all done on the type-level:

> type family SourceType dir rel :: *
> type instance SourceType L  (Rel entities c1 t1 c2 t2) = t1
> type instance SourceType R  (Rel entities c1 t1 c2 t2) = t2

> type family TargetType dir rel :: *
> type instance TargetType L  (Rel entities c1 t1 c2 t2) = t2
> type instance TargetType R  (Rel entities c1 t1 c2 t2) = t1

> type family TargetCardinality dir rel :: *
> type instance TargetCardinality L  (Rel entities c1 t1 c2 t2) = c2
> type instance TargetCardinality R  (Rel entities c1 t1 c2 t2) = c1

When we create a new entity we need to store the initial relationships. We 
find those initial relationships by 
building a filter function on the type-level. We filter out all the 
to-one relationship sets that apply to the given entity type.
For example, in the case of creating a new |Release| entity, the only included
relationship has to be |releases|.

The  difficult part is that a
given entity type can be on either side of the relationship set. Therefore, we split 
up our function into two parts. |InitialValues| looks for |r| on the left-hand 
side of the relationship set, while |InitialValues'| looks for |r| on the
right-hand side of the relationship set. The |originalRels| is needed so we can 
create pointers into the original list of all relationship sets.

> type family   InitialValues   entities r rels originalRels :: *
> type family   InitialValues'  entities r rels originalRels :: *

The base case is the empty list of relationship sets:

> type instance InitialValues entities r Nil  o = Nil

When we find a to-many relationship we call |InitialValues'| to see if the other
direction of the relationship matches.

> type instance InitialValues entities r (Rel entities c1 from Many to :*: xs) o = 
>   InitialValues' entities r (Rel entities c1 from Many to :*: xs) o

However, when we find a to-one relationship we include it in our |InitialValues| if 
the types |r| and |from| are equal, using the type-level functions
|AppendIfTrue| and  |TypeEq|. The |TypeEq| function compares two types and
returns |True| if the types are equal, and |False| otherwise. The |AppendIfTrue|
function takes three parameters: the first parameter is a boolean expression (at
the type-level), the second parameter is an element and the third parameter is a
list. If the first parameter is true, it returns a new list that combines the
second parameter and the third parameter. If the first parameter is false, the
result is the third parameter.
We also encode the direction |L| in which it was found.

> type instance InitialValues entities r (Rel entities c1 from One  to :*: xs) o = 
>   AppendIfTrue  (TypeEq r from) 
>                 (InitialValue    entities r L (Rel entities c1 from One to) o) 
>                 (InitialValues'  entities r   (Rel entities c1 from One to :*: xs) o)

The |InitialValues'| function is very similar, it looks for |r| in a different direction by comparing it with |to| instead of |from|:

> type instance InitialValues' entities r (Rel entities One from c1  to :*: xs) o = 
>   AppendIfTrue  (TypeEq r to)  
>                 (InitialValue entities r R (Rel entities One from c1 to) o) 
>                 (InitialValues entities r xs o)
> 
> type instance InitialValues' entities r (Rel entities Many from c1 to :*: xs) o = 
>   InitialValues entities r xs o

An |InitialValue| for a relationship contains a reference to the target entity,
the direction of the relationship and the index of the relationship set into all
relationship sets in the domain model. 
We carry the |Dir| argument around explicitly so that we can pattern-match on it in the next module.

> type InitialValue entities r dir rel rels =  (  Ref entities (TargetType dir rel)
>                                              ,  Dir dir
>                                              ,  Ix rels rel) 

> type family    Value entities cardinality typ :: *
> type instance  Value entities One         t   = Ref entities t
> type instance  Value entities Many        t   = S.Set (Ref entities t)
