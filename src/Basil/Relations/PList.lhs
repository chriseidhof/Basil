%if False

> {-# LANGUAGE KindSignatures, GADTs, TypeFamilies, TypeOperators #-}

> module Basil.Relations.PList where

> import Basil.Core
> import Basil.Relations.InitialValues
> import Basil.Data.TList

%endif

A |PList| stores the |InitialValues| for a given entity type. For example, in
the case of the |Release| entity type, it stores a |Ref| to a |Compiler|.
It is parameterized by |phi|, which is the type level list of all entities in a
model.
The variable |r| denotes the entity type that is created.
The variable |env| keeps track of all the initial values in the |PList|. It is a
type-level list that grows at each |PCons|.
Finally, |rels| is the type-level list that describes all the relationship sets in the
ER model.

> data PList phi r env rels where

The constructor |PNil| builds an empty list:

>  PNil   ::  PList phi r Nil rels

The |PCons| takes an |InitialValue| for the |SourceType| of the relation and
combines that with another |PList| that contains elements of the same
|SourceType|.
The |SourceType| of a |Rel phi c1 t1 c2 t2| can be either |t1| or
|t2|.

>  PCons  ::  (Rel phi c1 t1 c2 t2 ~ rel, src ~ SourceType dir rel)
>         =>  InitialValue phi src dir rel rels 
>         ->  PList phi  src env rels 
>         ->  PList phi  src 
>                        (InitialValue phi src dir rel rels :*: env)
>                        rels

\todo{Give an example |Plist|} value.
