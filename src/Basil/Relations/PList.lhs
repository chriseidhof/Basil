%if False

> {-# LANGUAGE KindSignatures, GADTs, TypeFamilies   #-}

> module Basil.Relations.PList where

> import Basil.Core
> import Basil.Relations.InitialValues

%endif

A |PList| stores the |InitialValues| for a given relationship. It is parameterized by
|phi|, which is the entity set domain. The variable |r| denotes the entity type
that is created. |env| keeps track of all the initial values in the |PList|. It
is a type-level list that grows at each |PCons|. Finally,
|rels| is a parameter that describes all the relationship sets in the ER model. It is
necessary to ensure an |InitialValue| always points to a valid relationship set
in the ER model. 

> data PList (phi :: * -> *) r env rels where

The constructor |PNil| builds an empty list:

>  PNil   ::  PList phi r () rels

The |PCons| takes an |InitialValue| for the |SourceType| of the relation and combines that with another |PList| that contains elements of the same |SourceType|. The |SourceType| of a |Rel phi c1 t1 c2 t2| can be either |t1| or |t2|.

>  PCons  ::  (Rel phi c1 t1 c2 t2 ~ rel, src ~ SourceType dir rel)
>         =>  InitialValue phi src dir rel rels 
>         ->  PList phi  src env rels 
>         ->  PList phi  src 
>                        (InitialValue phi src dir rel rels, env)
>                        rels
