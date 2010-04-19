Entity sets are naturally translated to table schemas, where every attribute in an entity set corresponds to an attribute in the table schema.
However, as we have noted in section \ref{sec:rdbschema}, not every attribute type is supported in database schemas.
Therefore, we provide a type-class to convert between attributes in the ER-model and attributes in a |RDBMS| table.
We provide default instances, and if an attribute can not be converted using the default instances, the compiler will give an error.
The user of our library can then provide an instance for her datatype.

%if False

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, TypeSynonymInstances, GADTs, ExistentialQuantification #-}

> module Basil.Database.Relational.Entities where
>
> import Basil.Database.Relational.Core
>
> import Generics.Regular
> import Basil.Data.TList hiding ((:*:))
> import qualified Basil.Data.TList as T
> import Prelude hiding ((.), id)
> import Control.Category
> import Data.Record.Label
> import Basil.Database.Relational.Utils

%endif

The functions in this section are defined using the \emph{Regular}\footnote{\url{http://hackage.haskell.org/package/regular}} generic programming library.
To convert an entity type into a table schema we convert each individual field to an attribute.
However, because |Attribute| types are restricted to a certain set of types (capture by the |Base| type from the previous section), we need a way to convert from and to |Base| types.
We do this by providing a class |Representable| to convert from and to base types.
We provide default instances for the |Base| types |Bool|, |String| and |Int|.
The function |baseCode| takes an |a| parameter but never inspects it, it is there to convince the type checker that we use the right instance of |Representable|.

> class Representable a rep | a -> rep where
>   baseCode     :: a -> Base rep
>   toRep        :: a -> rep
>   fromRep      :: rep -> a

%if False

> instance Representable String String where baseCode   _ = String; toRep = id; fromRep = id
> instance Representable Int    Int    where baseCode   _ = Int;    toRep = id; fromRep = id
> instance Representable Bool   Bool   where baseCode   _ = Bool;   toRep = id; fromRep = id

%endif

To express that we can convert an |f| into an attribute, we define the class |GAttr|.

> class GAttr f attr | f -> attr where
>  gattr      :: f a -> Attr env attr
>  gAttrBij   :: f a :<->: attr

There is only one instance, for a record field containing type |K a|. As a constraint, we require that |a| is an instance of |Representable|.

> instance (Selector s, Representable a rep) => GAttr (S s (K a)) rep where
>  gattr s    = Attr (selName s) (baseCode (unK $ unS s))
>  gAttrBij   = (toRep . unK . unS) <-> (S . K . fromRep)

To derive a schema generically, we define the class |GSchema|.
The type parameter |f| uniquely determines the |schema|, which is expressed using a functional dependency.

> class GSchema f schema | f -> schema where
>  gschema    :: f a -> Schema env schema
>  gbijection :: f a :<->: HList schema

We ignore the constructor wrapper:

> instance GSchema f schema => GSchema (C c f) schema where
>   gschema ~(C f)   = gschema f
>   gbijection       = let (Bijection l r) = gbijection
>                      in (l . unC) <-> (C . r)

In regular, the structure of a record type is encoded as nested pairs of |S| constructors. 
The last element is simply an |S| value, there is no |Nil| terminator. Therefore, we need to provide an instance for a single |S| constructor and an instance for products of |S| constructors. Note that |:*:| is used by both |Regular| and our |HList| library.
Therefore, we prefix the our |:*:| type constructor with a |T|.

> instance (GAttr (S s f) attr) => GSchema (S s f) (attr T.:*: Nil) where
>   gschema f   = gattr f .**. Nil2
>   gbijection  = let (Bijection l r) = gAttrBij
>                 in (\x -> l x .*. Nil) <-> (r . hHead)
>
> instance     (GAttr f attr, GSchema g gSchema) 
>          =>  GSchema  (f :*: g) (attr T.:*: gSchema) where
>   gschema ~(s :*: rest) = gattr s .**. gschema rest
>   gbijection = let (Bijection l r)   = gAttrBij
>                    (Bijection l' r') = gbijection
>                in  (\(x :*: xs) -> l x .*. l' xs) <-> (\(Cons x xs) -> r x :*: r' xs)

> class GName f where
>   gName :: f a -> String

> instance (Constructor c) => GName (C c f) where
>   gName = lower . conName

Finally, we define a function that builds a schema after converting an |a| to its structural representation.

> schema' :: (Regular a, GSchema (PF a) schema) => a -> Schema env schema
> schema' = gschema . from

> table' :: (Regular a, GName (PF a), GSchema (PF a) schema) => a -> TableT a
> table' x = TableT (Table (gName $ from x) $ gschema (from x)) bij
>  where (Bijection l r) = gbijection
>        bij = l . from <-> to . r

> data TableT a =  forall env schema. TableT 
>   { unTableT :: (Table env schema)
>   , trans    :: (a :<->: HList schema)
>   }

Converting schemas for all entities

> class ToSchema entities where
>   toSchema :: Witnesses finalEnv entities -> HList2 TableT entities

> instance ToSchema Nil where
>   toSchema WNil = Nil2

> instance (ToSchema b, Regular a, GName (PF a), GSchema (PF a) schema) => ToSchema (a T.:*: b) where
>   toSchema (WCons _ xs) = (table' undefined) .**. (toSchema xs)

Operations on entities:

> data Operation env result where
>   Create  :: Ix env ent -> ent  -> Operation env Int
>   Read    :: Ix env ent -> Int  -> Operation env (Maybe ent)
>   Update  :: Ix env ent -> Int  -> ent -> Operation env ()
>   Delete  :: Ix env ent -> Int  -> Operation env ()
