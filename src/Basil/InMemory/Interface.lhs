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
> import Basil.Data.TList (Ix, modTList, lookupTList, lookupMapTList, Witnesses)
> import Basil.Data.TList4
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

> data BasilState phi rels where
>   BasilState :: ERModel phi rels
>              => Cache phi 
>              -> RelCache rels
>              -> Int 
>              -> BasilState phi rels

We introduce a type synonym |Basil| that is a state monad with |BasilState| as its state.

> type Basil phi rels a =   ERModel phi rels 
>                       =>  ST.State (BasilState phi rels) a

%if False

> instance (Show (RelCache rels), Show (Cache phi )) => Show (BasilState phi rels) where
>   show (BasilState x y z) = "BasilState {" ++ unwords [show x, show y, show z] ++ "}"

%endif

We can now define a |find| method that searches the state for the entity of type |entity|, given a reference to it.

> find :: Ref phi entity -> Basil phi rels (Maybe entity)
> find (Ref tix entity)  =    M.lookup entity . get cached . lookupMapTList tix
>                        <$>  getM cache

Creating a new entitiy is a bit more involved. This is where our library shines: we not only ask for a value of |entity|, but also ask for all its |InitialValues| (see section \ref{sec:initialvalues}). This way, we make sure that all the right relationships are added.

> new  ::  Ix phi entity
>      ->  entity 
>      ->  PList phi entity (InitialValues phi entity rels rels) rels 
>      ->  Basil phi rels (Ref phi entity)

First, we will get a fresh integer that can be used for creating a reference. We then store the entity, and finally, the relationships. 

> new tix i rels = do 
>   freshId <- getM freshVariable
>   modM freshVariable (+1)
>   let  ident          = Fresh freshId
>        ref            = Ref tix ident
>   let  saveData       = mod  cached   (M.insert ident i)
>   modM cache    (modTList (saveData) tix)
>   modM relCache (storeAll ref rels)
>   return ref





Finally, we can provide a |runBasil| method that executes an in-memory database expression, yielding an |a| and the resulting state:

> runBasil  ::  forall phi rels a . ERModel phi rels
>           =>  Basil phi rels a 
>           ->  (a, BasilState phi rels)
> runBasil comp = ST.runState comp  (BasilState  (emptyState (witnesses :: Witnesses phi phi)) 
>                                                (empty (relations :: TList4 Rel rels))
>                                                0
>                                   )

We now have achieved the goals stated in the introduction of this section: we can store and find both entities and relationships, while maintaining the soundness of the relationship sets.

%endif

%if query

Querying the database is comparable to |find|, defined in section \ref{sec:inmeminterface}. The function |query| looks up the map with entities, filters the map by |cond| and finally converts it into a list. The function |eval| is used to compile the |Expr| value into a Haskell function with type |entity -> Bool|.

> query :: Ix phi entity -> Expr entity Bool -> Basil phi rels [(Ref phi entity, entity)]
> query tix cond  =  let look  =  mapFst (Ref tix) 
>                              .  M.toList 
>                              .  M.filter (eval cond) 
>                              .  get cached . lookupMapTList tix
>                     in look <$> getM cache

%endif


%if False

> mapFst f = map (\(x,y) -> (f x, y))

An instance for the |Persistent| typeclass:

> attr :: Ref phi entity -> (entity :-> att) -> Basil phi rels att
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
> cache :: BasilState phi rels :-> Cache phi 
> cache = label getCache setCache
>  where getCache :: BasilState phi rels -> Cache phi 
>        getCache (BasilState x _ _) = x
>        setCache :: Cache phi -> BasilState phi rels -> BasilState phi rels
>        setCache x (BasilState _ y z) = BasilState x y z
> freshVariable :: BasilState phi rels :-> Int
> freshVariable = label get' set'
>  where get' :: BasilState phi rels -> Int
>        get' (BasilState _ _ x) = x
>        set' :: Int -> BasilState phi rels -> BasilState phi rels
>        set' z (BasilState x y _) = BasilState x y z
> 
> relCache ::(ERModel phi rels) => BasilState phi rels :-> RelCache rels
> relCache = label get' set'
>  where get' :: (ERModel phi rels) => BasilState phi rels -> RelCache rels
>        get' (BasilState _ x _) = x
>        set' :: (ERModel phi rels) => RelCache rels -> BasilState phi rels -> BasilState phi rels
>        set' y (BasilState x _ z) = BasilState x y z

%endif
