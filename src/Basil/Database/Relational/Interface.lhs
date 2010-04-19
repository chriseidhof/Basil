%if False

> {-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, Rank2Types, FlexibleContexts #-}
> module Basil.Database.Relational.Interface where
>
> import Basil.Core
> import Data.Record.Label
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities
> import Basil.Database.Relational.Relationships
> import Basil.Database.Relational.Operations
> import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
> import Control.Monad.Trans (lift)
> import qualified Control.Monad.State as ST

%endif

> data BasilDBState entities res where
>   BasilDBState :: 
>      { _conn   :: Connection
>      , _tables :: HList2 TableT newTables
>      , _liftOp :: forall r . Operation entities r -> Operation newTables r
>      } -> BasilDBState entities res

> type BasilDB entities rels a =   ERModel entities rels 
>                              =>  ST.StateT (BasilDBState entities rels) IO a

> runBasilDB :: ( ToSchema entities
>               , ERModel entities rels
>               , AddRelationship rels entities tables
>               )
>            => BasilDB entities rels a -> String -> IO a
> runBasilDB db s = do 
>    st <- emptyState s
>    ST.evalStateT db st

> create  :: Ix entities ent -> ent  -> BasilDB entities rels Int
> create ix row = runOp $ Create ix row

> emptyState :: forall entities rels tables . 
>               ( ToSchema entities
>               , ERModel entities rels
>               , AddRelationship rels entities tables
>               ) 
>            => String -> IO (BasilDBState entities rels)
> emptyState s = let  rels    = relations :: TList4 Rel rels
>                     tables  = addRelationships (toSchema (witnesses :: Witnesses entities entities))
>                                                rels
>                     op      = liftOperations rels :: Operation entities r -> Operation tables r
>                in do conn <- connectSqlite3 s 
>                      return $ BasilDBState conn tables op
 
> find    :: Ix entities ent -> Int        -> BasilDB entities rels (Maybe ent)
> find ix i = runOp $ Read ix i

> update  :: Ix entities row -> Int        -> row -> BasilDB entities rels ()
> update ix i r = runOp $ Update ix i r

> delete  :: Ix entities row -> Int        -> BasilDB entities rels ()
> delete ix i = runOp $ Delete ix i

> runOp :: Operation entities a -> BasilDB entities rels a
> runOp op = do (BasilDBState conn tables liftOp) <- ST.get
>               lift $ runTables conn tables (liftOp op)

> runTables :: Connection -> HList2 TableT tables -> Operation tables r -> IO r
> runTables sess tables (Create ix row) = 
>   case lookupHList2 ix tables of
>     (TableT t transform)    -> do
>         create' sess t (fw transform row)
>     (RelTableT (Table nm s)) -> undefined nm s
> runTables sess tables (Read ix x) = 
>   case lookupHList2 ix tables of
>     (TableT t transform)    -> do
>         res <- find' sess t x 
>         return (fmap (bw transform) res)
>     (RelTableT (Table nm s)) -> undefined nm s
>
> runTables _ _ _ = error "not implemented yet."
