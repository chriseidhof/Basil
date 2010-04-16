Entity sets are naturally translated to table schemas, where every attribute in an entity set corresponds to an attribute in the table schema.
However, as we have noted in section \ref{sec:rdbschema}, not every attribute type is supported in database schemas.
Therefore, we provide a type-class to convert between attributes in the ER-model and attributes in a |RDBMS| table.
We provide default instances, and if an attribute can not be converted using the default instances, the compiler will give an error.
The user of our library can then provide an instance for her datatype.

%if False

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, TypeOperators, TypeSynonymInstances #-}

> module Basil.Database.Relational.Entities where
>
> import Basil.Database.Relational.Core
>
> import Generics.Regular
> import Basil.Data.TList hiding ((:*:))
> import qualified Basil.Data.TList as T


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
>  gattr :: f a -> Attr env attr

There is only one instance, for a record field containing type |K a|. As a constraint, we require that |a| is an instance of |Representable|.

> instance (Selector s, Representable a rep) => GAttr (S s (K a)) rep where
>  gattr s = Attr (selName s) (baseCode (unK $ unS s))

To derive a schema generically, we define the class |GSchema|.
The type parameter |f| uniquely determines the |schema|, which is expressed using a functional dependency.

> class GSchema f schema | f -> schema where
>  gschema :: f a -> Schema env schema

We ignore the constructor wrapper:

> instance GSchema f schema => GSchema (C c f) schema where
>   gschema (C f) = gschema f

In regular, the structure of a record type is encoded as nested pairs of |S| constructors. 
The last element is simply an |S| value, there is no |Nil| terminator. Therefore, we need to provide an instance for a single |S| constructor and an instance for products of |S| constructors. Note that |:*:| is used by both |Regular| and our |HList| library.
Therefore, we prefix the our |:*:| type constructor with a |T|.

> instance (GAttr (S s f) attr) => GSchema (S s f) (attr T.:*: Nil) where
>   gschema f = gattr f .**. Nil2
>
> instance     (GAttr f attr, GSchema g gSchema) 
>          =>  GSchema (f :*: g) (attr T.:*: gSchema) where
>   gschema (s :*: rest) = gattr s .**. gschema rest

Finally, we define a function that builds a schema after converting an |a| to its structural representation.

> schema' :: (Regular a, GSchema (PF a) schema) => a -> Schema env schema
> schema' = gschema . from
