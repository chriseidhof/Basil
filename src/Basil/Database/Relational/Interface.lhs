%if False

> {-# LANGUAGE ScopedTypeVariables, GADTs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
> {-# LANGUAGE TypeOperators, Rank2Types, FlexibleContexts, TypeFamilies #-}
> module Basil.Database.Relational.Interface where
>
> import Basil.Core
> import Basil.References
> import Basil.InMemory.Relations.Storage (Value (..)) -- TODO: wrong.
> import Data.Record.Label
> import Basil.Data.TList
> import Basil.Data.TList4
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Entities
> import Basil.Database.Relational.Relationships
> import Basil.Database.Relational.Operations
> import Basil.Relations.InitialValues
> import Database.HDBC 
> import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
> import Control.Monad.Trans (lift)
> import qualified Control.Monad.State as ST
> import qualified Data.Set as S
> import Unsafe.Coerce (unsafeCoerce)
> import Basil.Relations.PList

%endif

> data BasilDBState entities res where
>   BasilDBState :: 
>      { _conn    :: Connection
>      , _tables  :: HList2 TableT newTables
>      , _liftOp  :: forall r . Operation entities r -> Operation newTables r
>      , _liftRel :: forall rel . Ix res rel -> Ix newTables (RelTable rel)
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
>    x <- ST.evalStateT db st
>    commit (_conn st)
>    disconnect (_conn st)
>    return x

> new  :: Ix entities entity
>         -> entity  
>         -> PList entities entity (InitialValues entities entity rels rels) rels 
>         -> BasilDB entities rels (Ref entities entity)
> new ix row rels = do x <- fmap (Ref ix . Fresh) $ runOp $ Create ix row
>                      storeAll x rels
>                      return x

> storeAll :: Ref entities entity
>          -> PList entities entity env rels 
>          -> BasilDB entities rels ()
> storeAll r PNil         = return ()
> storeAll r (PCons x xs) = storeRelationship r x >> storeAll r xs

> storeRelationship :: Ref entities entity 
>                   -> InitialValue entities src dir (Rel entities c1 l c2 r) rels 
>                   -> BasilDB entities rels ()
> storeRelationship (Ref _ (Fresh r1)) ((Ref _ (Fresh r2)), dir, ix) = do
>  (BasilDBState conn tables liftOp liftRel) <- ST.get
>  let tableT = lookupHList2 (liftRel ix) tables
>      rel    = lookupTList4 ix relations
>  case (dir, tableT) of
>    (DL, TableT table f) -> do let row = (fw f $ ForeignKey r1 .*. ForeignKey r2 .*.  Nil)
>                               lift $ create' conn table row 
>                               return ()
>    (DR, TableT table f) -> do let row = (fw f $ ForeignKey r2 .*. ForeignKey r1 .*.  Nil)
>                               lift $ create' conn table row 
>                               return ()

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
>                     liftRel = unsafeCoerce              
>                in do conn <- connectSqlite3 s 
>                      return $ BasilDBState conn tables op liftRel
 
> find    :: Ref entities ent ->  BasilDB entities rels (Maybe ent)
> find (Ref ix (Fresh i)) = runOp $ Read ix i

> update  :: Ix entities row -> Int        -> row -> BasilDB entities rels ()
> update ix i r = runOp $ Update ix i r

> delete  :: Ix entities row -> Int        -> BasilDB entities rels ()
> delete ix i = runOp $ Delete ix i

> findRels  ::  ( cTarget  ~ TargetCardinality dir rel
>               , tTarget  ~ TargetType        dir rel
>               , source   ~ SourceType        dir rel
>               , rel ~ (Rel entities c1 l c2 r)
>               )
>           =>  Dir dir
>           ->  Ix rels rel
>           ->  Ref entities source
>           ->  BasilDB entities rels (Maybe (Value entities cTarget tTarget))
> findRels dir ix (Ref _ (Fresh x)) = do
>   (BasilDBState conn tables liftOp liftRel) <- ST.get
>   let tableT = lookupHList2 (liftRel ix) tables
>       rel    = lookupTList4 ix relations
>       cond   = condition dir
>   case tableT of
>     TableT table f -> do results <- lift $ findAll conn table (Just cond)
>                          return $ convertResults dir rel (map (bw f) results)
>                          
>  where condition :: Dir dir -> String
>        condition DL = "id_1 = " ++ int x
>        condition DR = "id_2 = " ++ int x
>        

> convertResults  ::  ( cTarget  ~ TargetCardinality dir rel
>                     , tTarget  ~ TargetType        dir rel
>                     , source   ~ SourceType        dir rel
>                     , rel ~ (Rel entities c1 l c2 r)
>                     )
>                 => Dir dir
>                 -> Rel entities c1 l c2 r
>                 -> [HList (Foreign l :*: Foreign r :*: Nil)]
>                 -> (Maybe (Value entities cTarget tTarget))
> convertResults DL  rel@(Rel One  _  _ One  ix _)  ((Cons x _         ):_) = Just (Ref ix $ Fresh $ foreignKey x)
> convertResults DR  rel@(Rel One  ix _ One  _  _)  ((Cons _ (Cons x _)):_) = Just (Ref ix $ Fresh $ foreignKey x)
> convertResults DL  rel@(Rel One  _  _ Many  ix _) ls  = Just $ S.fromList $
>   map (Ref ix . Fresh . foreignKey . lookupTList (Suc Zero)) ls
> convertResults DR  rel@(Rel One  ix _ Many  _ _) (x:xs)  = Just $ 
>   (Ref ix (Fresh (foreignKey (lookupTList Zero x))))
> convertResults DL  rel@(Rel Many  _  _ Many  ix _) ls  = Just $ S.fromList $
>   map (Ref ix . Fresh . foreignKey . lookupTList (Suc Zero)) ls
> convertResults DR  rel@(Rel Many  ix  _ Many  _ _) ls  = Just $ S.fromList $
>   map (Ref ix . Fresh . foreignKey . lookupTList (Zero)) ls
> convertResults _   _                             _       = Nothing
> -- convertResults DR  rel@(Rel One  ix _ Many  _  _) ((Cons _ (Cons x _)):_) = Just (Ref ix $ Fresh $ foreignKey x)

convertResults dir (Rel One  _ _ Many _ _) = undefined
convertResults dir (Rel Many _ _ One  _ _) = undefined
convertResults dir (Rel Many _ _ Many _ _) = undefined

> createDatabase :: BasilDB entities rels ()
> createDatabase = do
>   (BasilDBState conn tables liftOp liftRel) <- ST.get
>   lift $ createDatabase' conn tables 

> createDatabase' :: Connection -> HList2 TableT newTables -> IO ()
> createDatabase' conn tables = do
>   mapMHList2 (createTable' conn) tables
>   return ()

> runOp :: Operation entities a -> BasilDB entities rels a
> runOp op = do (BasilDBState conn tables liftOp liftRel) <- ST.get
>               lift $ runTables conn tables (liftOp op)

> runTables :: Connection -> HList2 TableT tables -> Operation tables r -> IO r
> runTables sess tables (Create ix row) = 
>   case lookupHList2 ix tables of
>     (TableT t transform)    -> create' sess t (fw transform row)
> runTables sess tables (Read ix x) = 
>   case lookupHList2 ix tables of
>     (TableT t transform)    -> do
>         res <- find' sess t x 
>         return (fmap (bw transform) res)
> runTables sess tables (Update ix i row) = 
>   case lookupHList2 ix tables of
>     (TableT t transform)    -> update' sess t i  (fw transform row)
>
> runTables _ _ _ = error "not implemented yet."
