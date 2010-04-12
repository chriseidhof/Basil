%if False 

> {-# LANGUAGE TypeFamilies, ScopedTypeVariables, UndecidableInstances,
>              Rank2Types, GADTs #-}
> module Basil.InMemory.Relations.Storage where
> 
> import Basil.InMemory.Cache
> import Basil.Relations.InitialValues
> import Basil.Core
> import Basil.References
> import Basil.Data.TList
> import Basil.Data.TList4
> import qualified Data.Map as M
> import qualified Data.Set as S
> import qualified Data.Maybe as Maybe
> import qualified Debug.Trace as D
> import Prelude hiding (lookup)


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

A many-to-many relationship is stored as two |Map| datatypes.
This allows for quick lookup.
A disadvantage is that it takes more memory, but as we only store references, we
expect that it will not be a problem in practice.

\begin{figure}

> type  family     RelationStorage rel :: *
> type  instance   RelationStorage (Rel entities  One  r1  One   r2)  = 
>                  M.Map   (Ref entities r1)  (Ref entities r2)
> type  instance   RelationStorage (Rel entities  One  r1  Many  r2)  = 
>                  M.Map   (Ref entities r2)  (Ref entities r1)
> type  instance   RelationStorage (Rel entities  Many r1  One   r2)  = 
>                  M.Map   (Ref entities r1)  (Ref entities r2)
> type  instance   RelationStorage (Rel entities  Many r1  Many  r2)  = 
>                  (  M.Map  (Ref entities r1  )  (S.Set (Ref entities r2))
>                  ,  M.Map  (Ref entities r2  )  (S.Set (Ref entities r1)))

\caption{The |RelationStorage| function}
\label{tfun:RelationStorage}
\end{figure}

All our relationship types |rels| are stored as a type-level list. Therefore, we
can apply the technique from the previous section, and build an |HList| with a
|RelationStorageN| datatype for each relationship type. 

> type RelCache rels = HList (TMap RelationStorageN rels)

Again, |RelationStorageN| is a newtype wrapping |RelationStorage| because
Haskell does not support partially applied type families (similar to the
partially applied |type| declarations in the previous section) :

> newtype RelationStorageN a = RelationStorageN { unRelationStorageN :: RelationStorage a}

Given a relationship set, we can create an empty datastructure for it. We will
add the suffix |S| to a function to indicate that we are dealing with functions
for just one relationship set. Functions on all relationship
sets in an ER model will not have this suffix.

> emptyS :: Rel entities c1 r1 c2 r2 -> RelationStorage (Rel entities c1 r1 c2 r2)
> emptyS (Rel  One   _  _  One   _ _) = M.empty
> emptyS (Rel  One   _  _  Many  _ _) = M.empty
> emptyS (Rel  Many  _  _  One   _ _) = M.empty
> emptyS (Rel  Many  _  _  Many  _ _) = (M.empty, M.empty)

%if False

We can now map over the list of all relationship sets |rels| to create an empty
datastructure for each relationship set in the ER model. We give its type, but
omit its definition. The |TList4| data structure is explained in section
\ref{sec:tlist4}

> empty :: TList4 Rel rels -> RelCache rels
> empty = fromTList4 (RelationStorageN . emptyS)

The |fromTList4| function is much like |map|, it lifts an |f| into a |g| structure
that is indexed by that |f|. In the code above, the |f| is the |Rel| type and
the |g| is the type |RelationStorageN|.

> fromTList4  ::  (forall a b c d entities . f entities a b c d -> g (f entities a b c d)) 
>             ->  TList4 f rels 
>             ->  HList (TMap g rels)
> fromTList4 f TNil4                = Nil
> fromTList4 f (TCons4 rel xs)  = f rel .*. fromTList4 f xs

%endif

Given two references and a relationship set we can insert the relationship into
the |RelationStorage| for that specific relationship set. By pattern-matching on
the cardinality inside the |Rel| datatype we provide the compiler with enough
information to discover the type of the data-structure for that cardinality.
Figure \ref{fun:insertS} shows the |insertS| function that creates a
relationship.

\begin{figure}

> insertS  ::  Ref entities l 
>          ->  Ref entities r 
>          ->  Rel entities c1 l c2 r 
>          ->  RelationStorage (Rel entities c1 l c2 r) 
>          ->  RelationStorage (Rel entities c1 l c2 r)
> insertS l r (Rel  One   _  _  One   _  _) s        =  M.insert l r s
> insertS l r (Rel  One   _  _  Many  _  _) s        =  M.insert r l s
> insertS l r (Rel  Many  _  _  One   _  _) s        =  M.insert l r s
> insertS l r (Rel  Many  _  _  Many  _  _) (s1,s2)  =  
>     (  M.alter (Just . maybe (S.singleton r)  (S.insert r))  l s1
>     ,  M.alter (Just . maybe (S.singleton l)  (S.insert l))  r s2
>     )

\caption{The |insertS| function}
\label{fun:insertS}
\end{figure}

We can now lift that function to the storage of all relationship sets in a
model. This looks up the right |RelationStorage| datatype using the |ix| value,
and changes it using the |insertS| function.

> insert  ::  ERModel entities rels
>         =>  Ix rels (Rel entities c1 l c2 r) 
>         ->  Ref entities l 
>         ->  Ref entities r
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

> type family    Value entities cardinality typ :: *
> type instance  Value entities One         t   = Ref entities t
> type instance  Value entities Many        t   = S.Set (Ref entities t)

Now we can write the |lookupS| function that looks up all the relationships.
Note that this function is quite inefficient for the one-to-many relationship.
Of course, the data-structures defined above could be changed to be more database-like.
However, this in-memory database is just a proof of concept, and choosing highly
efficient data-structures is beyond the scope of this thesis.

> lookupS  ::  Ref entities l 
>          ->  Rel entities c1 l c2 r 
>          ->  RelationStorage (Rel entities c1 l c2 r) 
>          ->  Maybe (Value entities c2 r)
> lookupS l (Rel  One   _  _  One   _ _) = M.lookup l
> lookupS l (Rel  One   _  _  Many  _ _) = Just . S.fromList . M.keys . M.filter (== l)
> lookupS l (Rel  Many  _  _  One   _ _) = M.lookup l
> lookupS l (Rel  Many  _  _  Many  _ _) = M.lookup l . fst

Our module also provides a |lookupS'| function that works in the other direction
of the relationship and has a very similar definition. Note that only the types
of the |Ref| and the result value have changed:

> lookupS'  ::  Ref entities r 
>           ->  Rel entities c1 l c2 r 
>           ->  RelationStorage (Rel entities c1 l c2 r) 
>           ->  Maybe (Value entities c1 l)

%if False

> lookupS' r (Rel One  _ _ One  _ _) = Maybe.listToMaybe . M.keys . M.filter (== r)
> lookupS' r (Rel One  _ _ Many _ _) = M.lookup r
> lookupS' r (Rel Many _ _ One  _ _) = Just . S.fromList . M.keys . M.filter (== r)
> lookupS' r (Rel Many _ _ Many _ _) = M.lookup r . snd

%endif

We can again lift both |lookupS| and |lookupS'|, which work on individual
relationship sets, to all relationship sets in an ER model:

> lookupLeft  ::  ERModel entities rels
>             =>  Ix rels (Rel entities c1 l c2 r)
>             ->  Ref entities l 
>             ->  RelCache rels
>             ->  Maybe (Value entities c2 r)
> lookupLeft = gLookup lookupS

%if False

> lookupRight  ::  ERModel entities rels 
>              =>  Ix rels (Rel entities c1 l c2 r)
>              ->  Ref entities r 
>              ->  RelCache rels
>              ->  Maybe (Value entities c1 l)
> lookupRight = gLookup lookupS'

%endif

The functions |lookupLeft| and |lookupRight| are so similar that we define a helper
function |gLookup|, which does the heavy lifting:

> gLookup  ::  ERModel entities rels
>          =>  (t -> ix -> RelationStorage ix -> c)
>          ->  Ix rels ix
>          ->  t
>          ->  HList (TMap RelationStorageN rels)
>          ->  c
> gLookup lookupFunc ix r  =  lookupFunc r (lookupTList4 ix relations) 
>                          .  unRelationStorageN 
>                          .  lookupMapTList ix

The function |lookup| dispatches to either |lookupLeft| or |lookupRight| based on the
direction. This will become useful once we provide an interface to the user.

> lookup ::    (  ERModel entities rels
>              ,  cTarget  ~ TargetCardinality dir rel
>              ,  tTarget  ~ TargetType        dir rel
>              ,  source   ~ SourceType        dir rel
>              ,  rel ~ (Rel entities c1 l c2 r)
>              )
>           => Dir dir
>           -> Ix rels rel
>           -> Ref entities source
>           -> RelCache rels
>           -> Maybe (Value entities cTarget tTarget)
> lookup DL = lookupLeft
> lookup DR = lookupRight

We now have defined a basic interface for storing relationships. We have built a
function that creates an empty datastructure, a function that inserts into the
datastructure and a function that does a lookup.
Again, functions for modifying and deleting relationships are similar to insert
and lookup.


%if False

For debugging, it's handy to have |Show| instances.

> instance Show (RelationStorage a) => Show (RelationStorageN a) where show = show . unRelationStorageN


%endif

%}
