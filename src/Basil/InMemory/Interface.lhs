%if False

> {-# LANGUAGE ScopedTypeVariables   #-}
> {-# LANGUAGE FlexibleContexts   #-}
> {-# LANGUAGE Rank2Types   #-}
> {-# LANGUAGE TypeOperators   #-}
> {-# LANGUAGE GADTs   #-}
> {-# LANGUAGE UndecidableInstances   #-}
> module Basil.InMemory.Interface (runBasil, find, new, query, attr, {- getRelation, setRelation, -} Basil (), BasilState) where
> 
> import Basil.Core
> import Basil.Query
> import Basil.InMemory.Cache
> import Basil.Relations
> import Basil.Relations.PList
> import Basil.References
> import Basil.Data.TBoolean
> import Basil.Data.TList (TIndex, modTList, lookupTList, EnumTypes, Witnesses, index, allTypes)
> import Basil.Data.TList4 (TList4)
> import Control.Applicative hiding (empty)
> import Generics.MultiRec.Base hiding (index)
> import qualified Control.Monad.State as ST
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Basil.Interface as P
> import Data.Record.Label hiding (set)
> import Prelude hiding (mod)

%endif

%if not query

We will now combine the storage of relations and the storage of entities to build the actual in-memory database. For convenience, we introduce a |BasilState| datatype that also stores an |Int| value that is a fresh-variable supply for creating new entities.

> data BasilState phi env rels where
>   BasilState :: (EnumTypes phi env, ERModel phi rels)
>              => Cache phi env 
>              -> RelCache phi rels
>              -> Int 
>              -> BasilState phi env rels

We introduce a type synonym |Basil| that is a state monad with |BasilState| as its state.

> type Basil phi env rels a =   (EnumTypes phi env, ERModel phi rels, TEq phi) 
>                           =>  ST.State (BasilState phi env rels) a

%if False

> instance (Show (RelCache phi rels), Show (Cache phi env)) => Show (BasilState phi env rels) where
>   show (BasilState x y z) = "BasilState {" ++ unwords [show x, show y, show z] ++ "}"

%endif

We can now define a |find| method that searches the state for the entity of type |entity|, given a reference to it.

> find :: (El phi entity) => Ref phi entity -> Basil phi env rels (Maybe entity)
> find (Ref tix entity)  =    M.lookup entity . get cached . lookupTList (index tix) 
>                        <$>  getM cache

Creating a new entitiy is a bit more involved. This is where our library shines: we not only ask for a value of |entity|, but also ask for all its |InitialValues| (see section \ref{sec:initialvalues}). This way, we make sure that all the right relationships are added.

> new  ::  (El phi entity) 
>      =>  entity 
>      ->  PList phi entity (InitialValues phi entity rels rels) rels 
>      ->  Basil phi env rels (Ref phi entity)

First, we will get a fresh integer that can be used for creating a reference. We then store the entity, and finally, the relationships. 

> new i rels = do  let tix = proof
>                  freshId <- getM freshVariable
>                  modM freshVariable (+1)
>                  let  ident          = Fresh freshId
>                       ref            = Ref tix ident
>                  let  saveData       = mod  cached   (M.insert ident i)
>                  modM cache    (modTList (saveData) (index tix))
>                  modM relCache (storeAll ref rels)
>                  return ref





Finally, we can provide a |runBasil| method that executes an in-memory database expression, yielding an |a| and the resulting state:

> runBasil  ::  forall phi env rels a . (TEq phi, EnumTypes phi env, ERModel phi rels) 
>           =>  Basil phi env rels a 
>           ->  (a, BasilState phi env rels)
> runBasil comp = ST.runState comp  (BasilState  (emptyState (allTypes :: Witnesses phi env)) 
>                                                (empty (relations :: TList4 Rel phi rels))
>                                                0
>                                   )

We now have achieved the goals stated in the introduction of this section: we can store and find both entities and relationships, while maintaining the soundness of the relationship sets.

%endif

%if query

Querying the database is comparable to |find|, defined in section \ref{sec:inmeminterface}. The function |query| looks up the map with entities, filters the map by |cond| and finally converts it into a list. The function |eval| is used to compile the |Expr| value into a Haskell function with type |entity -> Bool|.

> query :: (El phi entity) => (Expr entity Bool) -> Basil phi env rels [(Ref phi entity, entity)]
> query cond  =  let  tix   =  proof
>                     look  =  mapFst (Ref tix) 
>                           .  M.toList 
>                           .  M.filter (eval cond) 
>                           .  get cached . lookupTList (index tix) 
>                in look <$> getM cache

%endif


%if False

> mapFst f = map (\(x,y) -> (f x, y))

An instance for the |Persistent| typeclass:

> attr :: (El phi entity) => Ref phi entity -> (entity :-> att) -> Basil phi env rels att
> attr r@(Ref tix entity) at = do val <- find r
>                                 case val of
>                                      Just x  -> return $ get at x
>                                      Nothing -> error "Not found in cache."
> 


> 
> --setRelation :: (TEq phi, ERModel phi rels, Persist p phi) 
> --              => Ref phi r ->  phi r dir (Rel phi m1 m2 i1 i2) rels
> --     -> Basil phi env rels p ()
> --setRelation ref rel = modM relCache (setValue ref rel)
> --
> --getRelation :: (TEq phi, ERModel phi rels, Persist p phi) 
> --              => Ref phi r 
> --              -> (Dir dir, TIndex phi (Rel phi m1 m2 i1 i2) rels) 
> --              -> Basil phi env rels p (Maybe (Value dir (Rel phi m1 m2 i1 i2)))
> --getRelation ref rel = fmap (getValue ref rel) $ getM relCache
> 
> 
> -- State helper functions
> cache :: (EnumTypes phi env) => (BasilState phi env rels :-> Cache phi env)
> cache = label getCache setCache
>  where getCache :: (EnumTypes phi env) => BasilState phi env rels -> Cache phi env
>        getCache (BasilState x _ _) = x
>        setCache :: (EnumTypes phi env) => Cache phi env -> BasilState phi env rels -> BasilState phi env rels
>        setCache x (BasilState _ y z) = BasilState x y z
> freshVariable ::(EnumTypes phi env) => BasilState phi env rels :-> Int
> freshVariable = label get' set'
>  where get' :: BasilState phi env rels -> Int
>        get' (BasilState _ _ x) = x
>        set' :: (EnumTypes phi env) => Int -> BasilState phi env rels -> BasilState phi env rels
>        set' z (BasilState x y _) = BasilState x y z
> 
> relCache ::(ERModel phi rels) => BasilState phi env rels :-> RelCache phi rels
> relCache = label get' set'
>  where get' :: (ERModel phi rels) => BasilState phi env rels -> RelCache phi rels
>        get' (BasilState _ x _) = x
>        set' :: (ERModel phi rels) => RelCache phi rels -> BasilState phi env rels -> BasilState phi env rels
>        set' y (BasilState x _ z) = BasilState x y z

%endif
