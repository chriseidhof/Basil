%if False

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types, GADTs, EmptyDataDecls #-}
> module Basil.Database.InMemory.Relations where
>
> import Basil.Core
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.InMemory.Relations.Storage
> import Basil.Relations.InitialValues
> import Basil.Relations.PList
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe

%endif


The function |storeAll| takes a reference to |r| and a |PList| containing the
initial values for the relationships involving |r|, and stores them by applying
|setValue| on each |InitialValue|. For example, if |r| is the |Entity| type, the
|PList| contains a single element with a reference to a |Compiler| value.

> storeAll  ::  ERModel entities rels
>           =>  Ref entities r
>           ->  PList entities r env rels
>           ->  RelCache rels
>           ->  RelCache rels
> storeAll ref PNil          = id
> storeAll ref (PCons x xs)  = setValue ref x . storeAll ref xs

The function |setValue| stores a single relationship between a reference and an
initial value.
Based on the direction |DL| or |DR|, stored in the |InitialValue|, it can determine the right
order of the arguments.

> setValue  ::  ERModel entities rels
>           =>  Ref entities (SourceType dir (Rel entities c1 t1 c2 t2))
>           ->  InitialValue entities r dir (Rel entities c1 t1 c2 t2) rels
>           ->  RelCache rels
>           ->  RelCache rels
> setValue  x (y,DL,  ix)  = insert ix x y
> setValue  x (y,DR,  ix)  = insert ix y x
