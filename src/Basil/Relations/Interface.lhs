%if False

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls #-}
> module Basil.Relations.Interface where
>
> import Basil.Cache
> import Basil.Core
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Relations.Base
> import Basil.Relations.InitialValues
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe

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

The function |storeAll| takes a reference to |r| and a |PList| containing the initial values for the relationships involving |r|, and stores them by applying |setValue| on each |InitialValue|.


> storeAll  ::  (TEq phi, ERModel phi rels)
>           =>  Ref phi r
>           ->  PList phi r env rels
>           ->  RelCache phi rels
>           ->  RelCache phi rels
> storeAll ref PNil          = id
> storeAll ref (PCons x xs)  = setValue ref x . storeAll ref xs

The function |setValue| simply a relationship between a reference and an initial value. Based on the direction stored in the |InitialValue| it can determine the right order of the arguments.

> setValue  :: (TEq phi, ERModel phi rels)
>           =>  Ref phi (SourceType dir (Rel phi c1 t1 c2 t2))
>           ->  InitialValue phi r dir (Rel phi c1 t1 c2 t2) rels
>           ->  RelCache phi rels
>           ->  RelCache phi rels
> setValue  x (y,DL,  ix)  = insert ix x y
> setValue  x (y,DR,  ix)  = insert ix y x

%}
