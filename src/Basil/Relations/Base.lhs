> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances, Rank2Types #-}
> module Basil.Relations.Base where
> 
> import Basil.Cache
> import Basil.Core
> import Basil.References
> import Basil.Data.TList
> import Basil.Data.TList4
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe
> import qualified Debug.Trace as D

This is the base module where we define how to store relationships. We only consider relationships between two entities with a cardinality of one-to-one, one-to-many, many-to-one or many-to-many. Relationships are grouped into relationship sets, where all relationships are between the same entity types and have the same cardinality.

> -- | The storage for a specific relationship set
> type  family    RelationStorage rel :: *
> type  instance  RelationStorage (Rel phi One  r1 One  r2)  = M.Map   (Ref phi r1)  (Ref phi r2)
> type  instance  RelationStorage (Rel phi One  r1 Many r2)  = M.Map   (Ref phi r2)  (Ref phi r1)
> type  instance  RelationStorage (Rel phi Many r1 One  r2)  = M.Map   (Ref phi r1)  (Ref phi r2)
> type  instance  RelationStorage (Rel phi Many r1 Many r2)  = (M.Map  (Ref phi r1)  (S.Set (Ref phi r2)), M.Map (Ref phi r2) (S.Set (Ref phi r1)))

For an ER model, we enumerate all the relationship sets on the type-level using nested pairs that are growing to the right. We can write a function on those type-level lists that computes how relationships for an entire ER model are stored:

> type RelCache phi rels = TList RelationStorageN phi rels

|RelationStorageN| is just a newtype to convice the compiler.

> newtype RelationStorageN a = RelationStorageN { unRelationStorageN :: RelationStorage a}


Given a relationship set, we can create an empty datastructure for it. We will
add the suffix S to a function to indicate that we are dealing with functions
for just one relationship set. Functions on all relationship
sets in an ER model will not have this suffix.

> emptyS :: Rel phi c1 r1 c2 r2 -> RelationStorage (Rel phi c1 r1 c2 r2)
> emptyS (Rel One  _ _ One  _ _) = M.empty
> emptyS (Rel One  _ _ Many _ _) = M.empty
> emptyS (Rel Many _ _ One  _ _) = M.empty
> emptyS (Rel Many _ _ Many _ _) = (M.empty, M.empty)


We can map over the list of all relationship sets |rels| to create an empty
datastructure for each relationship set in the ER model:

> empty :: TList4 Rel phi rels -> RelCache phi rels
> empty relations = worker (RelationStorageN . emptyS) relations
> 
> worker :: (forall c1 c2 r1 r2 . f phi c1 c2 r1 r2 -> g (f phi c1 c2 r1 r2)) -> TList4 f phi rels -> TList g phi rels
> worker f TNil4 = ()
> worker f (TCons4 _ _ rel xs) = (f rel, worker f xs)


Given two references and a relationship set we can update the |RelationStorage|
for that specific relationship set. By pattern-matching on the |Rel| datatype we
can alter the storage for that specific relationship set.

> insertS :: Ref phi l 
>         -> Ref phi r 
>         -> Rel phi c1 l c2 r 
>         -> RelationStorage (Rel phi c1 l c2 r) 
>         -> RelationStorage (Rel phi c1 l c2 r)
> insertS l r (Rel One  _ _ One  _ _) s       =  M.insert l r s
> insertS l r (Rel One  _ _ Many _ _) s       =  M.insert r l s
> insertS l r (Rel Many _ _ One  _ _) s       =  M.insert l r s
> insertS l r (Rel Many _ _ Many _ _) (s1,s2) =  ( M.alter (Just . maybe (S.singleton r) (S.insert r)) l s1
>                                                , M.alter (Just . maybe (S.singleton l) (S.insert l)) r s2
>                                                )

We can now lift that function to the storage of all relationship sets in a
model:

> insert :: forall phi rels c1 l c2 r . (ERModel phi rels) 
>        => TIndex phi (Rel phi c1 l c2 r) rels 
>        -> Ref phi l 
>        -> Ref phi r
>        -> RelCache phi rels
>        -> RelCache phi rels
> insert ix l r = modTList (RelationStorageN . insertS l r (lookupTList4 ix relations) . unRelationStorageN) ix

Another essential operation is |lookup|. Given a reference to an entity and a
relationship set, we want to find all matching relationships. In a one-to-one
relationship set this will be exactly one relationship. In a one-to-many it will
be a list of refences. Before we define |lookup|, we will express its return type using 
the |Value| type-family:

> type family Value (phi :: * -> *) cardinality typ :: *
> type instance Value phi One   t = Ref phi t
> type instance Value phi Many  t = S.Set (Ref phi t)

Now we can write the |lookupS| function that looks up all the relationships.

> lookupS :: Ref phi l -> Rel phi c1 l c2 r -> RelationStorage (Rel phi c1 l c2 r) -> Maybe (Value phi c2 r)
> lookupS l (Rel One  _ _ One  _ _) = M.lookup l
> lookupS l (Rel One  _ _ Many _ _) = Just . S.fromList . M.keys . M.filter (== l)
> lookupS l (Rel Many _ _ One  _ _) = M.lookup l
> lookupS l (Rel Many _ _ Many _ _) = M.lookup l . fst

We also provide a |lookupS'| function that works in the other direction of the
relationship:

> lookupS' :: Ref phi r -> Rel phi c1 l c2 r -> RelationStorage (Rel phi c1 l c2 r) -> Maybe (Value phi c1 l)
> lookupS' r (Rel One  _ _ One  _ _) = Maybe.listToMaybe . M.keys . M.filter (== r)
> lookupS' r (Rel One  _ _ Many _ _) = M.lookup r
> lookupS' r (Rel Many _ _ One  _ _) = Just . S.fromList . M.keys . M.filter (== r)
> lookupS' r (Rel Many _ _ Many _ _) = M.lookup r . snd

We can again lift these functions on individual relationship sets to all
relationship sets in an ER model:

> lookup :: forall phi rels c1 l c2 r . (ERModel phi rels) 
>        => TIndex phi (Rel phi c1 l c2 r) rels 
>        -> Ref phi l 
>        -> RelCache phi rels
>        -> Maybe (Value phi c2 r)
> lookup = gLookup lookupS

> lookup' :: forall phi rels c1 l c2 r . (ERModel phi rels) 
>        => TIndex phi (Rel phi c1 l c2 r) rels 
>        -> Ref phi r 
>        -> RelCache phi rels
>        -> Maybe (Value phi c1 l)
> lookup' = gLookup lookupS'

> gLookup
>   :: (ERModel phi env) 
>   => (t -> ix -> RelationStorage ix -> c)
>   -> TIndex phi ix env
>   -> t
>   -> TList RelationStorageN phi env
>   -> c
> gLookup lookupFunc ix r = lookupFunc r (lookupTList4 ix relations) . unRelationStorageN . lookupTList ix

For debugging, it's handy to have |Show| instances.

> instance Show (RelationStorage a) => Show (RelationStorageN a) where show = show . unRelationStorageN
