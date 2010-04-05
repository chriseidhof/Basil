%if False 

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances,
>              Rank2Types, GADTs #-}
> module Basil.InMemory.Relations.Storage where
> 
> import Basil.InMemory.Cache
> import Basil.Core
> import Basil.References
> import Basil.Data.TList
> import Basil.Data.TList4
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe
> import qualified Debug.Trace as D


This is the base module where we define how to store relationships. 

%endif
%{

%format One  = "\mathbf{One}"
%format Many = "\mathbf{Many}"

We only consider relationships between two entities with a cardinality of
one-to-one, one-to-many, many-to-one or many-to-many. Relationships are grouped
into relationship sets, where all relationships are between the same entity
types and have the same cardinality. Based on the cardinality, we can choose a
datatype to store the relationships. We do this using a type-level function
|RelationStorage|, defined in Figure \ref{tfun:RelationStorage}. Because |One| and |Many| play an important
role in this section, they are highlighted.

\begin{figure}
> type  family     RelationStorage rel :: *
> type  instance   RelationStorage (Rel phi  One  r1  One   r2)  = 
>                  M.Map   (Ref phi r1)  (Ref phi r2)
> type  instance   RelationStorage (Rel phi  One  r1  Many  r2)  = 
>                  M.Map   (Ref phi r2)  (Ref phi r1)
> type  instance   RelationStorage (Rel phi  Many r1  One   r2)  = 
>                  M.Map   (Ref phi r1)  (Ref phi r2)
> type  instance   RelationStorage (Rel phi  Many r1  Many  r2)  = 
>                  (  M.Map  (Ref phi r1  )  (S.Set (Ref phi r2))
>                  ,  M.Map  (Ref phi r2  )  (S.Set (Ref phi r1)))
\caption{The |RelationStorage| function}
\label{tfun:RelationStorage}
\end{figure}




For an ER model, we enumerate all the relationship sets on the type-level using
nested pairs that are growing to the right. We can reuse our |HList| datatype
for storing all relationship sets |rels| in an ER model |phi|.

> type RelCache rels = HList (TMap RelationStorageN rels)

Again, |RelationStorageN| is a newtype wrapping |RelationStorage| because
Haskell does not support partially applied type families (similar to the
partially applied |type| declarations in the previous section) :

> newtype RelationStorageN a = RelationStorageN { unRelationStorageN :: RelationStorage a}

Given a relationship set, we can create an empty datastructure for it. We will
add the suffix |S| to a function to indicate that we are dealing with functions
for just one relationship set. Functions on all relationship
sets in an ER model will not have this suffix.

> emptyS :: Rel phi c1 r1 c2 r2 -> RelationStorage (Rel phi c1 r1 c2 r2)
> emptyS (Rel  One   _  _  One   _ _) = M.empty
> emptyS (Rel  One   _  _  Many  _ _) = M.empty
> emptyS (Rel  Many  _  _  One   _ _) = M.empty
> emptyS (Rel  Many  _  _  Many  _ _) = (M.empty, M.empty)

%if False

We can map over the list of all relationship sets |rels| to create an empty
datastructure for each relationship set in the ER model. We give its type, but
omit its definition. The |TList4| data structure is explained in section
\ref{sec:tlist4}

> empty :: TList4 Rel rels -> RelCache rels
> empty = fromTList4 (RelationStorageN . emptyS)


The |fromTList4| function is much like |map|, it lifts an |f| into a |g| structure
that is indexed by that |f|:

> fromTList4  ::  (forall a b c d phi . f phi a b c d -> g (f phi a b c d)) 
>             ->  TList4 f rels 
>             ->  HList (TMap g rels)
> fromTList4 f TNil4                = Nil
> fromTList4 f (TCons4 rel xs)  = f rel .*. fromTList4 f xs

%endif

Given two references and a relationship set we can insert the relationship into
the |RelationStorage| for that specific relationshipset . By pattern-matching on
the cardinality inside the |Rel| datatype we provide the compiler with enough
information to discover the type of the data-structure for that cardinality.

> insertS  ::  Ref phi l 
>          ->  Ref phi r 
>          ->  Rel phi c1 l c2 r 
>          ->  RelationStorage (Rel phi c1 l c2 r) 
>          ->  RelationStorage (Rel phi c1 l c2 r)
> insertS l r (Rel  One   _  _  One   _  _) s        =  M.insert l r s
> insertS l r (Rel  One   _  _  Many  _  _) s        =  M.insert r l s
> insertS l r (Rel  Many  _  _  One   _  _) s        =  M.insert l r s
> insertS l r (Rel  Many  _  _  Many  _  _) (s1,s2)  =  
>     (  M.alter (Just . maybe (S.singleton r)  (S.insert r))  l s1
>     ,  M.alter (Just . maybe (S.singleton l)  (S.insert l))  r s2
>     )

We can now lift that function to the storage of all relationship sets in a
model. This looks up the right |RelationStorage| datatype using the |ix| value,
and changes it using the |insertS| function.

> insert  ::  ERModel phi rels
>         =>  Ix rels (Rel phi c1 l c2 r) 
>         ->  Ref phi l 
>         ->  Ref phi r
>         ->  RelCache rels
>         ->  RelCache rels
> insert ix l r =  modTList 
>                  ( withRelationStorageN (insertS l r (lookupTList4 ix relations)) )
>                  ix

The helper function |withRelationStorageN| unwraps the newtype, applies the
function and wraps it again:

> withRelationStorageN  ::  (RelationStorage a -> RelationStorage b)
>                       ->  RelationStorageN a
>                       ->  RelationStorageN b
> withRelationStorageN f = RelationStorageN . f . unRelationStorageN

Another essential operation is |lookup|. Given a reference to an entity and a
relationship set, we want to find all matching entities. In a one-to-one
relationship set this will be exactly one entity. In a one-to-many relationship it will
be a list of references. Before we define |lookup|, we will express its return type using 
the |Value| type-family:

> type family Value phi cardinality typ :: *
> type instance Value phi One   t = Ref phi t
> type instance Value phi Many  t = S.Set (Ref phi t)

Now we can write the |lookupS| function that looks up all the relationships.
Note that this function is quite inefficient for the one-to-many relationship.
Overall, the data-structures could be improved to be more database-like.
However, this in-memory database is just a proof of concept, but can probably be
made quite fast by changing the datastructures in the |RelationStorage|
type function on page \pageref{tfun:RelationStorage}.

> lookupS  ::  Ref phi l 
>          ->  Rel phi c1 l c2 r 
>          ->  RelationStorage (Rel phi c1 l c2 r) 
>          ->  Maybe (Value phi c2 r)
> lookupS l (Rel  One   _  _  One   _ _) = M.lookup l
> lookupS l (Rel  One   _  _  Many  _ _) = Just . S.fromList . M.keys . M.filter (== l)
> lookupS l (Rel  Many  _  _  One   _ _) = M.lookup l
> lookupS l (Rel  Many  _  _  Many  _ _) = M.lookup l . fst

Our module also provides a |lookupS'| function that works in the other direction
of the relationship and has a very similar definition. Note that only the types
of the |Ref| and the result value have changed:

> lookupS'  ::  Ref phi r 
>           ->  Rel phi c1 l c2 r 
>           ->  RelationStorage (Rel phi c1 l c2 r) 
>           ->  Maybe (Value phi c1 l)

%if False

> lookupS' r (Rel One  _ _ One  _ _) = Maybe.listToMaybe . M.keys . M.filter (== r)
> lookupS' r (Rel One  _ _ Many _ _) = M.lookup r
> lookupS' r (Rel Many _ _ One  _ _) = Just . S.fromList . M.keys . M.filter (== r)
> lookupS' r (Rel Many _ _ Many _ _) = M.lookup r . snd

%endif

We can again lift both |lookupS| and |lookupS'|, which work on individual
relationship sets, to all relationship sets in an ER model:

> lookup  ::  ERModel phi rels
>         =>  Ix rels (Rel phi c1 l c2 r)
>         ->  Ref phi l 
>         ->  RelCache rels
>         ->  Maybe (Value phi c2 r)
> lookup = gLookup lookupS

%if False

> lookup'  ::  ERModel phi rels 
>          =>  Ix rels (Rel phi c1 l c2 r)
>          ->  Ref phi r 
>          ->  RelCache rels
>          ->  Maybe (Value phi c1 l)
> lookup' = gLookup lookupS'

%endif

The functions |lookup| and |lookup'| are so similar that we define a helper
function |gLookup|, which does the heavy lifting:

> gLookup  ::  ERModel phi rels
>          =>  (t -> ix -> RelationStorage ix -> c)
>          ->  Ix rels ix
>          ->  t
>          ->  HList (TMap RelationStorageN rels)
>          ->  c
> gLookup lookupFunc ix r  =  lookupFunc r (lookupTList4 ix relations) 
>                          .  unRelationStorageN 
>                          .  lookupMapTList ix

We now have defined a basic interface for storing relationships. We have built a
function that creates an empty datastructure, a function that inserts into the
datastructure and a function that does a lookup.

%if False

For debugging, it's handy to have |Show| instances.

> instance Show (RelationStorage a) => Show (RelationStorageN a) where show = show . unRelationStorageN

%endif

%}
