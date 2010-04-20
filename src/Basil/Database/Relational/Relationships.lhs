%if False

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances,
>              RankNTypes, ExistentialQuantification, TypeFamilies #-}
> module Basil.Database.Relational.Relationships where
>
> import Basil.Core
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities
> import Data.Record.Label
> import Unsafe.Coerce (unsafeCoerce)

%endif

> class AddRelationship rels tables newTables | rels tables -> newTables where
>   addRelationships :: HList2 TableT tables 
>                    -> TList4 Rel rels 
>                    -> HList2 TableT newTables
>   liftOperations :: TList4 Rel rels -> Operation tables r -> Operation newTables r

> type family   RelTable rel :: *
> type instance RelTable (Rel phi m1 x m2 y) = HList (Foreign x :*: Foreign y :*: Nil)

>    
>
> instance AddRelationship Nil tables tables where
>   addRelationships h TNil4 = h
>   liftOperations TNil4 = id

> instance ( AddRelationship b tables tables'
>          , rel ~ Rel phi m1 x m2 y
>          , table ~ RelTable rel
>          )
>         => AddRelationship (rel :*: b) tables (table :*: tables') where
>   addRelationships h (TCons4 x xs) = let h' = addRelationships h xs
>                                      in (toTable x .**. h')
>   liftOperations (TCons4 _ xs) = addedTable . liftOperations xs


> addedTable :: Operation env result -> Operation (x :*: env) result
> addedTable (Create ix row)    = Create (Suc ix) row
> addedTable (Read   ix x)      = Read   (Suc ix) x
> addedTable (Update ix x row)  = Update (Suc ix) x row
> addedTable (Delete ix x)      = Delete (Suc ix) x
> addedTable (FindAll ix)       = FindAll (Suc ix)

> toTable :: Rel phi m1 x m2 y -> TableT (HList (Foreign x :*: Foreign y :*: Nil))
> toTable (Rel cL ixL nL cR ixR nR) = TableT table bij
>  where table = (Table (nL ++ "_" ++ nR) $ Foreign "id_1" ixL .**. Foreign "id_2" ixR .**. Nil2)
>        bij  = id <-> id
