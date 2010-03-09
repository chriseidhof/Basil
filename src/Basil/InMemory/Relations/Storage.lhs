%if False 

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies , FlexibleInstances, UndecidableInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
>
> module Basil.InMemory.Relations.Storage where
> 
> import Basil.InMemory.EntityStorage
> import Basil.Core
> import Basil.References
> import Data.HList
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe


This is the base module where we define how to store relationships. 

%endif
%{

%format One  = "\mathbf{One}"
%format Many = "\mathbf{Many}"

We only consider relationships between two entities with a cardinality of
one-to-one, one-to-many, many-to-one or many-to-many. Relationships are grouped
into relationship sets, where all relationships are between the same entity
types and have the same cardinality. Based on the cardinality, we can choose a
datatype to store the relationships. Because |One| and |Many| play an important
role in this section, they are highlighted.

> class RelationStorage es c1 c2 t1 t2 store | es c1 c2 t1 t2 -> store where
>   emptyRelationStorage :: Rel es c1 c2 t1 t2 -> store
>   insertRelation :: Ref es t1 -> Ref es t2 -> store -> store

> instance RelationStorage es One One r1 r2
>                          (M.Map (Ref es r1) (Ref es r2)) 
>   where emptyRelationStorage _ = M.empty
>         insertRelation l r s   = M.insert l r s

> -- insertS l r (Rel  One   _  _  Many  _  _) s        =  M.insert r l s
> -- insertS l r (Rel  Many  _  _  One   _  _) s        =  M.insert l r s
> -- insertS l r (Rel  Many  _  _  Many  _  _) (s1,s2)  =  

> instance RelationStorage es One Many r1 r2
>                          (M.Map (Ref es r2) (Ref es r1)) 
>   where emptyRelationStorage _ = M.empty
>         insertRelation l r s   = M.insert r l s

> instance RelationStorage es Many One r1 r2
>                          (M.Map (Ref es r1) (Ref es r2)) 
>   where emptyRelationStorage _ = M.empty
>         insertRelation l r s   = M.insert l r s

> instance RelationStorage es Many Many r1 r2
>                          (  M.Map  (Ref es r1  )  (S.Set (Ref es r2))
>                          ,  M.Map  (Ref es r2  )  (S.Set (Ref es r1)))
>   where emptyRelationStorage _     = (M.empty, M.empty)
>         insertRelation l r (s1,s2) = (  M.alter (Just . maybe (S.singleton r)  (S.insert r))  l s1
>                                      ,  M.alter (Just . maybe (S.singleton l)  (S.insert l))  r s2
>                                      )

Type-level HMap

> data MkRelationStorage = MkRelationStorage

> instance (RelationStorage es c1 c2 t1 t2 store) => Apply MkRelationStorage (Rel es c1 c2 t1 t2) store where
>   apply MkRelationStorage x = emptyRelationStorage x

> class HMap MkRelationStorage relations storage => RelationsStorage relations storage where
>   relationsStorage :: relations -> storage


> instance ( ERModel entities relations
>          , HMap MkRelationStorage relations storage)
>          => RelationsStorage relations storage where
>   relationsStorage = hMap MkRelationStorage 

> class HMappedLookupByHNat ix ls res | ix ls -> res where
>  hMappedLookupByHNat :: ix -> ls -> res

> instance (HLookupByHNat ix ls src, HMap f ls ls', Apply f src res) => HMappedLookupByHNat ix ls' res where
>  hMappedLookupByHNat = undefined

Given two references and a relationship set we can insert the relationship into
the |RelationStorage| for that specific relationshipset . By pattern-matching on
the cardinality inside the |Rel| datatype we provide the compiler with enough
information to discover the type of the data-structure for that cardinality.

> insert :: ( RelationsStorage relations storage
>           , HLookupByHNat ix relations (Rel es c1 c2 l r)
>           , Apply MkRelationStorage (Rel es c1 c2 l r) store
>           )
>         => Ref entities l
>         -> Ref entities r
>         -> ix
>         -> storage -> storage
> insert l r ix s = let x = hMappedLookupByHNat ix s
>                   in hMappedUpdateAtHNat ix x s

> -- insertS  ::  Ref phi l 
> --          ->  Ref phi r 
> --          ->  Rel phi c1 l c2 r 
> --          ->  RelationStorage (Rel phi c1 l c2 r) 
> --          ->  RelationStorage (Rel phi c1 l c2 r)

-- 
-- We can now lift that function to the storage of all relationship sets in a
-- model:
-- 
-- > insert  ::  (ERModel phi rels) 
-- >         =>  TIndex phi (Rel phi c1 l c2 r) rels 
-- >         ->  Ref phi l 
-- >         ->  Ref phi r
-- >         ->  RelCache phi rels
-- >         ->  RelCache phi rels
-- > insert ix l r =  modTList 
-- >                  ( withRelationStorageN (insertS l r (lookupTList4 ix relations)) )
-- >                  ix
-- 
-- The helper function |withRelationStorageN| unwraps the newtype, applies the
-- function and wraps it again:
-- 
-- > withRelationStorageN  ::  (RelationStorage a -> RelationStorage b)
-- >                       ->  RelationStorageN a
-- >                       ->  RelationStorageN b
-- > withRelationStorageN f = RelationStorageN . f . unRelationStorageN
-- 
-- Another essential operation is |lookup|. Given a reference to an entity and a
-- relationship set, we want to find all matching entities. In a one-to-one
-- relationship set this will be exactly one entity. In a one-to-many relationship it will
-- be a list of references. Before we define |lookup|, we will express its return type using 
-- the |Value| type-family:
-- 
-- > type family Value (phi :: * -> *) cardinality typ :: *
-- > type instance Value phi One   t = Ref phi t
-- > type instance Value phi Many  t = S.Set (Ref phi t)
-- 
-- Now we can write the |lookupS| function that looks up all the relationships.
-- Note that this function is quite inefficient for the one-to-many relationship.
-- Overall, the data-structures could be improved to be more database-like.
-- However, this in-memory database is just a proof of concept, but can probably be
-- made quite fast by changing the datastructures in the |RelationStorage|
-- type function on page \pageref{tfun:RelationStorage}.
-- 
-- > lookupS  ::  Ref phi l 
-- >          ->  Rel phi c1 l c2 r 
-- >          ->  RelationStorage (Rel phi c1 l c2 r) 
-- >          ->  Maybe (Value phi c2 r)
-- > lookupS l (Rel  One   _  _  One   _ _) = M.lookup l
-- > lookupS l (Rel  One   _  _  Many  _ _) = Just . S.fromList . M.keys . M.filter (== l)
-- > lookupS l (Rel  Many  _  _  One   _ _) = M.lookup l
-- > lookupS l (Rel  Many  _  _  Many  _ _) = M.lookup l . fst
-- 
-- Our module also provides a |lookupS'| function that works in the other direction
-- of the relationship and has a very similar definition. Note that only the types
-- of the |Ref| and the result value have changed:
-- 
-- > lookupS'  ::  Ref phi r 
-- >           ->  Rel phi c1 l c2 r 
-- >           ->  RelationStorage (Rel phi c1 l c2 r) 
-- >           ->  Maybe (Value phi c1 l)
-- 
-- %if False
-- 
-- > lookupS' r (Rel One  _ _ One  _ _) = Maybe.listToMaybe . M.keys . M.filter (== r)
-- > lookupS' r (Rel One  _ _ Many _ _) = M.lookup r
-- > lookupS' r (Rel Many _ _ One  _ _) = Just . S.fromList . M.keys . M.filter (== r)
-- > lookupS' r (Rel Many _ _ Many _ _) = M.lookup r . snd
-- 
-- %endif
-- 
-- We can again lift both |lookupS| and |lookupS'|, which work on individual
-- relationship sets, to all relationship sets in an ER model:
-- 
-- > lookup  ::  ERModel phi rels
-- >         =>  TIndex phi (Rel phi c1 l c2 r) rels 
-- >         ->  Ref phi l 
-- >         ->  RelCache phi rels
-- >         ->  Maybe (Value phi c2 r)
-- > lookup = gLookup lookupS
-- 
-- %if False
-- 
-- > lookup'  ::  ERModel phi rels 
-- >          =>  TIndex phi (Rel phi c1 l c2 r) rels 
-- >          ->  Ref phi r 
-- >          ->  RelCache phi rels
-- >          ->  Maybe (Value phi c1 l)
-- > lookup' = gLookup lookupS'
-- 
-- %endif
-- 
-- The functions |lookup| and |lookup'| are so similar that we define a helper
-- function |gLookup|, which does the heavy lifting:
-- 
-- > gLookup  ::  ERModel phi env
-- >          =>  (t -> ix -> RelationStorage ix -> c)
-- >          ->  TIndex phi ix env
-- >          ->  t
-- >          ->  TList RelationStorageN phi env
-- >          ->  c
-- > gLookup lookupFunc ix r  =  lookupFunc r (lookupTList4 ix relations) 
-- >                          .  unRelationStorageN 
-- >                          .  lookupTList ix
-- 
-- We now have defined a basic interface for storing relationships. We have built a
-- function that creates an empty datastructure, a function that inserts into the
-- datastructure and a function that does a lookup.
-- 
-- %if False
-- 
-- For debugging, it's handy to have |Show| instances.
-- 
-- > instance Show (RelationStorage a) => Show (RelationStorageN a) where show = show . unRelationStorageN
-- 
-- %endif
-- 
-- %}
