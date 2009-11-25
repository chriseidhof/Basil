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
in the ER model. For readability, we have introduced a type-level let.

%format Myr = "\beta"
%format tlet = "\mathbf{tlet}"

\begin{spec}
tlet Myr = Rel phi c1 i1 c2 i2 in
\end{spec}

%format (Rel phi c1 i1 c2 i2) = "\beta"

> data PList (phi :: * -> *) r env rels where
>  PNil   ::  PList phi r () rels
>  PCons  ::  InitialValue phi (SourceType dir (Rel phi c1 i1 c2 i2)) dir (Rel phi c1 i1 c2 i2) rels 
>         ->  PList phi  (SourceType dir (Rel phi c1 i1 c2 i2)) env rels 
>         ->  PList phi  (SourceType dir (Rel phi c1 i1 c2 i2)) 
>                        (InitialValue phi  (SourceType dir (Rel phi c1 i1 c2 i2)) 
>                                           dir 
>                                           (Rel phi c1 i1 c2 i2) rels, env)
>                        rels



> storeAll :: (TEq phi, ERModel phi rels)
>          => Ref phi r
>          -> PList phi r env rels
>          -> RelCache phi rels
>          -> RelCache phi rels
> storeAll ref PNil          = id
> storeAll ref (PCons x xs)  = setValue ref x . storeAll ref xs

> setValue :: (TEq phi, ERModel phi rels)
>          => Ref phi (SourceType dir (Rel phi c1 i1 c2 i2))
>          -> InitialValue phi r dir (Rel phi c1 i1 c2 i2) rels
>          -> RelCache phi rels
>          -> RelCache phi rels
> setValue x (y,DL,ix) = insert ix x y
> setValue x (y,DR,ix) = insert ix y x
