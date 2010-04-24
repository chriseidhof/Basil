Entity sets are naturally translated to table schemas, where every attribute in an entity set corresponds to an attribute in the table schema.
However, as we have noted in section \ref{sec:rdbschema}, not every attribute type is supported in database schemas.
Therefore, we provide a type-class to convert between attributes in the ER-model and attributes in a table.
We give default instances, and if an attribute can not be converted using the default instances, the compiler displays an error message.
The user of our library can then provide an instance for her datatype that converts to one of the base attribute types.

%if False

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, TypeSynonymInstances, GADTs, ExistentialQuantification #-}

> module Basil.Database.Relational.Entities where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
>
> import Basil.Data.TList hiding ((:*:))
> import Control.Category
> import Data.Record.Label
> import Generics.Regular
> import Prelude hiding ((.), id)
> import qualified Basil.Data.TList as T

%endif

We provide default instances for the |Base| types |Bool|, |String| and |Int|.
The function |baseCode| takes an |a| parameter but never inspects it: it is there to convince the type checker that we use the right instance of |Representable|.

> class Representable a rep | a -> rep where
>   baseCode     :: a -> Base rep
>   toRep        :: a -> rep
>   fromRep      :: rep -> a

%if False

> instance Representable String String where baseCode   _ = String; toRep = id; fromRep = id
> instance Representable Int    Int    where baseCode   _ = Int;    toRep = id; fromRep = id
> instance Representable Bool   Bool   where baseCode   _ = Bool;   toRep = id; fromRep = id

%endif

To translate entities into table schemas we use generic programming \todo{references}.
The functions in this section are defined using the \emph{Regular} generic programming library. 
We refer to the library homepage\footnote{\url{http://hackage.haskell.org/package/regular}}
for documentation on how to write generic functions using Regular.

To express that we can convert an |f| into an attribute, we define the class
|GAttr|. It has a function |gattr| that builds the desired attribute. The |f a|
is again a value to convince the type checker, and is never inspected.
The function |gattrBij| is a
bijection between entity attribute values |f a| and table attribute values
|attr|.

> class GAttr f attr | f -> attr where
>  gattr      :: f a -> Attr env attr
>  gattrBij   :: f a :<->: attr

There is only one instance, for a record field containing type |K a|. As a constraint, we require that |a| is an instance of |Representable|.

> instance (Selector s, Representable a rep) => GAttr (S s (K a)) rep where
>  gattr s    = Attr (selName s) (baseCode (unK $ unS s))
>  gattrBij   = (toRep . unK . unS) <-> (S . K . fromRep)

To derive a schema generically, we define the class |GSchema|.
The type parameter |f| uniquely determines the |schema|, which is expressed
using a functional dependency. The |gschema| value builds a schema (without
inspecting |f a|), and the |gbijection| provides a bijection between |f a|
values and |HList schema| values. We can use the |gbijection| function to
convert between entities and table rows (in both directions).
We omit the instances, they can be found in the code accompanying this thesis.

> class GSchema f schema | f -> schema where
>  gschema     :: f a -> Schema env schema
>  gbijection  :: f a :<->: HList schema

%if False

> instance GSchema f schema => GSchema (C c f) schema where
>   gschema ~(C f)   = gschema f
>   gbijection       = let (Bijection l r) = gbijection
>                      in (l . unC) <-> (C . r)


In the regular library, the structure of a record type is encoded as nested pairs of |S| constructors. 
The last element is simply an |S| value, there is no |Nil| terminator. Therefore, we need to provide an instance for a single |S| constructor and an instance for products of |S| constructors. Note that |:*:| is used by both |Regular| and our |HList| library.
Therefore, we prefix the our |:*:| type constructor with a |T|.

> instance (GAttr (S s f) attr) => GSchema (S s f) (attr T.:*: Nil) where
>   gschema f   = gattr f .**. Nil2
>   gbijection  = let (Bijection l r) = gattrBij
>                 in (\x -> l x .*. Nil) <-> (r . hHead)
>
> instance     (GAttr f attr, GSchema g gSchema) 
>          =>  GSchema  (f :*: g) (attr T.:*: gSchema) where
>   gschema ~(s :*: rest) = gattr s .**. gschema rest
>   gbijection = let (Bijection l r)   = gattrBij
>                    (Bijection l' r') = gbijection
>                in  (\(x :*: xs) -> l x .*. l' xs) <-> (\(Cons x xs) -> r x :*: r' xs)

> class GName f where
>   gName :: f a -> String

> instance (Constructor c) => GName (C c f) where
>   gName = lower . conName

%endif

We define a datatype |TableT| that describes simple tables. It is indexed by
|a|, which means it can transform |a| values into |schema| values, and back.

> data TableT a =  forall env schema. TableT 
>   { unTableT  :: Table env schema
>   , trans     :: a :<->: HList schema
>   }

%if False

> table' :: (Regular a, GName (PF a), GSchema (PF a) schema) => a -> TableT a
> table' x = TableT (Table (gName $ from x) $ gschema (from x)) bij
>  where (Bijection l r) = gbijection
>        bij = l . from <-> to . r

> schema' :: (Regular a, GSchema (PF a) schema) => a -> Schema env schema
> schema' = gschema . from

%endif

We also provide a typeclass to generate all tables for entities. The |toSchema|
function takes a list of all the entity types, and produces an |HList2| with
|TableT| types, one for each entity.

> class ToSchema entities where
>   toSchema :: Witnesses finalEnv entities -> HList2 TableT entities

%if False

> instance ToSchema Nil where
>   toSchema WNil = Nil2

> instance (ToSchema b, Regular a, GName (PF a), GSchema (PF a) schema) => ToSchema (a T.:*: b) where
>   toSchema (WCons _ xs) = (table' undefined) .**. (toSchema xs)

%endif

Finally, we define operations on entities.
Instead of defining them as functions, we define them as a datatypes.
This allows us to modify them when needed.
The operations are indexed by |env|, which is the type-level list of tables.

> data Operation env result where
>   Create   :: Ix env ent -> ent  -> Operation env Int
>   Read     :: Ix env ent -> Int  -> Operation env (Maybe ent)
>   Update   :: Ix env ent -> Int  -> ent -> Operation env ()
>   Delete   :: Ix env ent -> Int  -> Operation env ()
>   FindAll  :: Ix env ent         -> Operation env [(Int, ent)]

At this point, we have seen how to automatically translate entities into relational database tables.
In the next section, we add a table for each relationship, and we transform |Operation| values to work on the new list of tables.
