%if False

> {-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, Rank2Types, FlexibleContexts #-}
> module Basil.Database.Relational.Interface where
>
> import Basil.Core
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.Relational.Entities
> import Basil.Database.Relational.Relationships
> import Database.Sqlite.Enumerator
> import qualified Control.Monad.State as ST

%endif

> data BasilDBState entities res = forall tables . BasilDBState
>  { _conn   :: ConnectA Session
>  , _tables :: HList2 SchemaT tables
>  , _liftOp :: Operation entities r -> Operation tables r
>  }

> type BasilDB entities rels a =   ERModel entities rels 
>                              =>  ST.State (BasilDBState entities rels) (IO a)

> create  :: Ix entities row -> HList row  -> BasilDB entities rels Int
> create ix row = runOp $ Create ix row

> emptyState :: ERModel entities rels => String -> (BasilDBState entities rels)
> emptyState s = let (tables, op) = addRelationships (toSchema (undefined :: HList entities))
>                                                    (relations :: TList4 Rel rels)
>                in  BasilDBState (connect s) tables op
 
read    :: Ix entities row -> Int        -> BasilDB (Maybe (HList row))
read = undefined

update  :: Ix entities row -> Int        -> HList row -> BasilDB ()
update = undefined

delete  :: Ix entities row -> Int        -> Operation env ()
delete = undefined

> runOp :: Operation entities a -> BasilDB entities rels a
> runOp op = do (BasilDBState conn tables lift) <- ST.get
>               return $ runTables conn tables (lift op)

> runTables :: ConnectA Session -> HList2 SchemaT tables -> Operation tables r -> IO r
> runTables = undefined
