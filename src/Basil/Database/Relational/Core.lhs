%if False

> {-# LANGUAGE GADTs, FlexibleContexts, EmptyDataDecls, UndecidableInstances #-}
> 
> module Basil.Database.Relational.Core where
> 
> import Basil.Data.TList

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

To model an attribute we define a datatype |Attr|, with two constructors.
The |Attr| constructor encodes a simple attribute by taking a name and a code in |Base|.
The |Foreign| constructor encodes a special attribute: a \emph{foreign key}.
In relational databases, a \emph{foreign key} is a pointer to a row in a
different table.

> data Attr tables t where
>   Attr     :: String  -> Base t       -> Attr tables t
>   Foreign  :: String  -> Ix tables t  -> Attr tables (Foreign t)

%if False

> instance Show (Attr tables t) where
>  show (Attr nm t)    = "Attr " ++ nm ++ show t
>  show (Foreign nm _) = "Foreign " ++ nm

%endif

The |Foreign| datatype is used to store foreign keys.
It provides a |ForeignKey| constructor that wraps an integer.

> newtype Foreign a = ForeignKey { foreignKey :: Int }

%if False

>  deriving Show

%endif

A table schema is a list of attributes, which we can express using our |HList2| datatype.
Every attribute is indexed with |tables|, the list of tables in our data model.
This way, we can ensure that foreign keys can only refer to a table in |tables|.

> type Schema tables atts = HList2 (Attr tables) atts

A table is simply a schema with a name.

> data Table tables atts = Table  {  tableName    ::  String
>                                 ,  tableSchema  ::  Schema tables atts
>                                 } 

%if False

> instance (Show (Schema tables atts)) => Show (Table tables atts) where
>  show (Table t s) = show (t, show s)

%endif
