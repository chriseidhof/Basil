%if False

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances,
>              RankNTypes, ExistentialQuantification, TypeFamilies #-}
> module Basil.Database.Relational.Relationships where
>
> import Basil.Core
> import Basil.Data.HList
> import Basil.Data.HList4
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities
> import Data.Record.Label

%endif

To store relationships, we take a database schema, and add a table for each
relationship. The table contains a foreign key for both entities. Note that
the resulting database schema is not normalized, this is future work.
Schema normalization can be seen as a transformation, and can probably be expressed using the TTTAS library \cite{tttas}.

To encode relationships as database tables, we define the |AdddRelationship| typeclass.
The |AddRelationship| type class has a parameter |rels|, which is the list of
relationships. Furthermore, it has two parameters |tables| and |newTables|,
which state that, given a list of |rels|, it converts a database schema with
type |tables| into a schema with type |newTables|. It does not only provide a
new list of tables, it also provides a function |liftOperations| that lifts an
operation on the original |tables| to an operation on the |newTables|. 

> class AddRelationship rels tables newTables | rels tables -> newTables where
>   addRelationships  ::  HList4 Rel rels 
>                     ->  HList2 TableT tables 
>                     ->  HList2 TableT newTables
>   liftOperations :: HList4 Rel rels -> Operation tables r -> Operation newTables r

We have given instances for the |AddRelationship| type class for both the |Nil|
and |:*:| types, which means that we can convert all lists of relationships.


%if False

> type family   RelTable rel :: *
> type instance RelTable (Rel phi m1 x m2 y) = HList (Foreign x :*: Foreign y :*: Nil)

> instance AddRelationship Nil tables tables where
>   addRelationships TNil4 h = h
>   liftOperations TNil4 = id

> instance ( AddRelationship b tables tables'
>          , rel ~ Rel phi m1 x m2 y
>          , table ~ RelTable rel
>          )
>         => AddRelationship (rel :*: b) tables (table :*: tables') where
>   addRelationships (TCons4 x xs) h = let h' = addRelationships xs h
>                                      in (toTable x .**. h')
>   liftOperations (TCons4 _ xs) = addedTable . liftOperations xs


> addedTable :: Operation env result -> Operation (x :*: env) result
> addedTable (Create ix row)    = Create (Suc ix) row
> addedTable (Read   ix x)      = Read   (Suc ix) x
> addedTable (Update ix x row)  = Update (Suc ix) x row
> addedTable (Delete ix x)      = Delete (Suc ix) x
> addedTable (FindAll ix)       = FindAll (Suc ix)

> toTable :: Rel phi m1 x m2 y -> TableT (HList (Foreign x :*: Foreign y :*: Nil))
> toTable (Rel _ ixL nL _ ixR nR) = TableT table bij
>  where table = (Table (nL ++ "_" ++ nR) $ Foreign "id_1" ixL .**. Foreign "id_2" ixR .**. Nil2)
>        bij  = id :<->: id

%endif
