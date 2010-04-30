> {-# LANGUAGE ScopedTypeVariables
>            , FlexibleContexts
>            , Rank2Types
>            , TypeOperators
>            , GADTs
>            , UndecidableInstances
>            , TypeFamilies
>            #-}
> module Basil.Database.InMemory.Interface (runBasil, contBasil, find, findAll, new, findRels,
>   query, attr, Basil (), BasilState,
>   emptyBasilState) where
> 
> import Basil.Core
> import Basil.Query
> import Basil.Relations
> import Basil.Database.InMemory.Cache
> import Basil.Database.InMemory.Relations.Storage
> import Basil.Database.InMemory.Relations
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.HList 
> import Basil.Data.HList4
> import Control.Applicative hiding (empty)
> import qualified Control.Monad.State as ST
> import qualified Data.Map as M
> import qualified Data.IntMap as I
> import qualified Data.Set as S
> import Data.Record.Label hiding (set)
> import Prelude hiding (mod, lookup)

In this section we provide an interface for the in-memory database that can store and modify entities and relationships. 
The final interface is summarized in section \ref{sec:inmemif}.

The datatype |BasilState| stores the entities, relationships and an |Int| value,
which is used as a fresh-variable supply for creating new entities.

> data BasilState entities rels = BasilState  
>    {  entities_       :: Cache entities
>    ,  relationships_  :: RelCache rels
>    ,  freshVariable_  :: Int
>    } 

Because all operations change the |BasilState| datatype, we wrap our operations in a |State| monad:

> type Basil entities rels a  =   ERModel entities rels 
>                             =>  ST.State (BasilState entities rels) a

%if False

> instance (Show (RelCache rels), Show (Cache entities )) => Show (BasilState entities rels) where
>   show (BasilState x y z) = "BasilState {" ++ unwords [show x, show y, show z] ++ "}"

%endif

To find an entity we define the |find| function, which looks up the entity in the |Map| containing the entities. The result is wrapped in a |Maybe| value, because the entity might not exist in the |Map|.

\label{sec:inmemfind}

> find :: Ref entities entity -> Basil entities rels (Maybe entity)
> find (Ref tix entity)  =    I.lookup entity . get cached . lookupMapHList tix
>                        <$>  getM cache


Creating a new entitiy is a bit more involved.
Not only do we ask for a value of |entity|, but also for all its |InitialValues| (see section \ref{sec:initialvalues}).
This way, we make sure that all the necessary relationships are added.
For example, the |new| function for the |Release| entity also asks for a |PList| that contains a |Compiler| entity.

\label{sec:inmemnew}

> new  ::  Ix entities entity
>      ->  entity 
>      ->  PList entities entity (InitialValues entities entity rels rels) rels 
>      ->  Basil entities rels (Ref entities entity)

First, we get a fresh integer that can be used for creating a reference.
We then store the entity, and relationships.
Finally, we return the newly created reference.

> new tix i rels = do 
>   ident <- getM freshVariable
>   modM freshVariable (+1)
>   let  ref            = Ref tix ident
>        saveData       = mod  cached   (I.insert ident i)
>   modM cache    (modHList (saveData) tix)
>   modM relCache (storeAll ref rels)
>   return ref

\label{sec:inmemfindrels}

To lookup the relationships for an entity and a relationship set, we provide the |findRels| function, which is defined in terms of the |lookup| function from section \ref{sec:inmemrels}. Based on the |TargetCardinality|, it can return a normal value or a |Set| with all references.

> findRels  ::  ( cTarget  ~ TargetCardinality dir rel
>               , tTarget  ~ TargetType        dir rel
>               , source   ~ SourceType        dir rel
>               , rel ~ (Rel entities c1 l c2 r)
>               )
>           =>  Dir dir
>           ->  Ix rels rel
>           ->  Ref entities source
>           ->  Basil entities rels (Maybe (Value entities cTarget tTarget))
> findRels dir ix ref = lookup dir ix ref <$> getM relCache



The |emptyBasilState| constructs an empty state for an ER-models.
The explicit |forall| quantification is used in combination with the ScopedTypeVariables language extension in order to bring the |entities| and |rels| type variables into scope.

> emptyBasilState :: forall entities rels . 
>                    ERModel entities rels 
>                 => BasilState entities rels
> emptyBasilState = (BasilState  (emptyState  (witnesses :: Witnesses entities entities)) 
>                                (empty       (relations :: HList4 Rel rels))
>                                0
>                                )

Finally, the |runBasil| method executes an in-memory database expression, yielding an |a| and the resulting state.

> runBasil  ::  ERModel entities rels
>           =>  Basil entities rels a 
>           ->  (a, BasilState entities rels)
> runBasil comp = ST.runState comp emptyBasilState

%if False

Also, to continue with a non-empty state:

> contBasil  ::  forall entities rels a . ERModel entities rels
>           =>  Basil entities rels a 
>           ->  BasilState entities rels
>           ->  (a, BasilState entities rels)
> contBasil comp s = ST.runState comp s

%endif

We have now defined an interface to the in-memory database that can create and find entities and relationships.
When creating an entity, it is guaranteed that the initial relationships have to be supplied. 

%endif

%if query

Querying the database is comparable to |find|, defined in section \ref{sec:inmeminterface}. The function |query| looks up the map with entities, filters the map by |cond| and finally converts it into a list. The function |eval| is used to compile the |Expr| value into a Haskell function with type |entity -> Bool|. For relational databases, we provide the same interface.

> query :: Ix entities entity -> Expr entity Bool -> Basil entities rels [(Ref entities entity, entity)]
> query tix cond  =  let look  =  mapFst (Ref tix) 
>                              .  I.toList 
>                              .  I.filter (eval cond) 
>                              .  get cached . lookupMapHList tix
>                     in look <$> getM cache


%endif



%if False

> findAll :: Ix entities entity -> Basil entities rels [(Ref entities entity, entity)]
> findAll tix  =  let look  =  mapFst (Ref tix) 
>                         .  I.toList 
>                         .  get cached . lookupMapHList tix
>                in look <$> getM cache

> mapFst f = map (\(x,y) -> (f x, y))

An instance for the |Persistent| typeclass:

> attr :: Ref entities entity -> (entity :-> att) -> Basil entities rels att
> attr r@(Ref tix entity) at = do val <- find r
>                                 case val of
>                                      Just x  -> return $ get at x
>                                      Nothing -> error "Not found in cache."
> 


> 
> --setRelation :: (TEq entities, ERModel entities rels, Persist p entities) 
> --              => Ref entities r ->  entities r dir (Rel entities m1 m2 i1 i2) rels
> --     -> Basil entities env rels p ()
> --setRelation ref rel = modM relCache (setValue ref rel)
> --
> --getRelation :: (TEq entities, ERModel entities rels, Persist p entities) 
> --              => Ref entities r 
> --              -> (Dir dir, TIndex entities (Rel entities m1 m2 i1 i2) rels) 
> --              -> Basil entities env rels p (Maybe (Value dir (Rel entities m1 m2 i1 i2)))
> --getRelation ref rel = fmap (getValue ref rel) $ getM relCache
> 
> 
> -- State helper functions
> cache :: BasilState entities rels :-> Cache entities 
> cache = label getCache setCache
>  where getCache :: BasilState entities rels -> Cache entities 
>        getCache (BasilState x _ _) = x
>        setCache :: Cache entities -> BasilState entities rels -> BasilState entities rels
>        setCache x (BasilState _ y z) = BasilState x y z
> freshVariable :: BasilState entities rels :-> Int
> freshVariable = label get' set'
>  where get' :: BasilState entities rels -> Int
>        get' (BasilState _ _ x) = x
>        set' :: Int -> BasilState entities rels -> BasilState entities rels
>        set' z (BasilState x y _) = BasilState x y z
> 
> relCache ::(ERModel entities rels) => BasilState entities rels :-> RelCache rels
> relCache = label get' set'
>  where get' :: (ERModel entities rels) => BasilState entities rels -> RelCache rels
>        get' (BasilState _ x _) = x
>        set' :: (ERModel entities rels) => RelCache rels -> BasilState entities rels -> BasilState entities rels
>        set' y (BasilState x _ z) = BasilState x y z

%endif
