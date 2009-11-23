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

A |PList| stores the |InitialValues| for a given relationship. It is parameterized by
|phi|, which is the entity set domain. The variable |r| denotes the entity type
that is created. |env| keeps track of all the initial values in the |PList|. Finally,
|rels| is a parameter that describes all the relationship sets in the ER model. It is
needed so that we can ensure an |InitialValue| always points to a valid relationship set
in the ER model.

> data PList (phi :: * -> *) r env rels where
>  PNil :: PList phi r () rels
>  PCons :: InitialValue phi (SourceType dir (Rel phi c1 i1 c2 i2)) dir (Rel phi c1 i1 c2 i2) rels 
>        -> PList phi (SourceType dir (Rel phi c1 i1 c2 i2)) env rels 
>        -> PList phi (SourceType dir (Rel phi c1 i1 c2 i2)) (InitialValue phi  (SourceType dir (Rel phi c1 i1 c2 i2)) dir (Rel phi c1 i1 c2 i2) rels, env) rels



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
> setValue x (y,dir,ix) = setValueFinder dir x y ix

> setValueFinder :: ERModel phi rels
>                => Dir dir
>                -> Ref phi (SourceType dir (Rel phi c1 t1 c2 t2))
>                -> Ref phi (TargetType dir (Rel phi c1 t1 c2 t2))
>                -> TIndex phi (Rel phi c1 t1 c2 t2) rels
>                -> RelCache phi rels
>                -> RelCache phi rels
> setValueFinder DL = setValueL
> setValueFinder DR = setValueR

> setValueL
>      :: (ERModel phi rels) 
>      => (Ref phi t1)
>      -> Ref phi i2
>      -> TIndex phi (Rel phi c1 t1 c2 i2) rels
>      -> RelCache phi rels
>      -> RelCache phi rels
> setValueL l  r relIx  = insert relIx l r

> setValueR
>      :: (ERModel phi rels) 
>      => Ref phi t2
>      -> Ref phi t1
>      -> TIndex phi (Rel phi c1 t1 c2 t2) rels
>      -> RelCache phi rels
>      -> RelCache phi rels
> setValueR l  r relIx  = insert relIx r l
