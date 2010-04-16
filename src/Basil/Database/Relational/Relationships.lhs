%if False

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances, RankNTypes, ExistentialQuantification #-}
> module Basil.Database.Relational.Relationships where
>
> import Basil.Core
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities

%endif

> class AddRelationship rels tables newTables | rels tables -> newTables where
>   addRelationships :: HList2 SchemaT tables 
>                    -> TList4 Rel rels 
>                    -> (HList2 SchemaT newTables, ConvertOp tables newTables)
>
> instance AddRelationship Nil tables tables where
>   addRelationships h TNil4 = (h, id)

> instance (AddRelationship b tables tables', ToTable (Rel phi m1 x m2 y) t)
>         => AddRelationship (Rel phi m1 x m2 y :*: b) tables (t :*: tables') where
>   addRelationships h (TCons4 x xs) = let (h', f') = addRelationships h xs
>                                      in (toTable x .**. h', addedTable . f')

> addedTable :: Operation env result -> Operation (x :*: env) result
> addedTable (Create ix row)    = Create (Suc ix) row
> addedTable (Read   ix x)      = Read   (Suc ix) x
> addedTable (Update ix x row)  = Update (Suc ix) x row
> addedTable (Delete ix x)      = Delete (Suc ix) x

> class ToTable r t where
>   toTable :: r -> SchemaT t

> instance ToTable (Rel phi m1 x m2 y) (Foreign x :*: Foreign y :*: Nil) where
>   toTable (Rel _ ixL _ _ ixR _) = RelSchemaT $ Foreign "id_1" ixL .**. Foreign "id_2" ixR .**. Nil2

