%if False

> {-# LANGUAGE GADTs, FlexibleContexts, EmptyDataDecls, UndecidableInstances #-}
> 
> module Basil.Database.Relational.Core where
> 
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Database.Enumerator
> import Database.Sqlite.Enumerator

> instance Show (Base a) where
>   show  String  = "<String>"
>   show  Int     = "<Int>"
>   show  Bool    = "<Bool>"

%endif

An attribute of a table describes its name and the domain. Because relational
databases support only a limited number of attribute types, we define a closed
set of types called |Base|, containing a \emph{code} for every such type:

> data Base t where
>   String  ::  Base String
>   Int     ::  Base Int
>   Bool    ::  Base Bool

A simple attribute can now be modeled by taking a name and a code in |Base|
A foreign key also requires that we have a pointer to a value in |tables|.
The type |tables| is a type-level list of all the tables in our model.


> data Attr tables t where
>   Attr     :: String  -> Base t       -> Attr tables t
>   Foreign  :: String  -> Ix tables t  -> Attr tables (Foreign t)

%if False

> instance Show (Attr tables t) where
>  show (Attr nm t)    = "Attr " ++ nm ++ show t
>  show (Foreign nm f) = "Foreign " ++ nm

%endif

The |Foreign| is a type-level value to indicate that something is a foreign key.
On the value level, a foreign key is represented as an |Int|.

> newtype Foreign a = ForeignKey { foreignKey :: Int }

A table schema is a list of attributes, which we can express using our |HList| datatype. Every attribute is indexed with |tables|, the list of tables in our data model.

> type Schema tables atts = HList2 (Attr tables) atts

A table is simply a schema with a name.

> data Table tables atts = Table String (Schema tables atts)

%if False

> instance (Show (Schema tables atts)) => Show (Table tables atts) where
>  show (Table t s) = show (t, show s)

%endif
