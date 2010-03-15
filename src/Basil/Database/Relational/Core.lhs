%if False

> {-# LANGUAGE GADTs, FlexibleContexts #-}
> 
> module Basil.Database.Relational.Core where
> 
> import Basil.Data.TBoolean
> import Basil.Data.TList
> import Database.Enumerator
> import Database.Sqlite.Enumerator

%endif

An attribute of a table describes its name and the domain. Because relational
databases support only a limited number of attribute types, we define a closed
set of types called |Base|, containing a code for every such type:

> data Base t where
>   String  ::  Base String
>   Int     ::  Base Int
>   Bool    ::  Base Bool

> instance Show (Base a) where
>   show String = "<String>"
>   show Int    = "<Int>"
>   show Bool   = "<Bool>"

An attribute can now be modeled by taking a name and a code in |Base|:

> data Attr stmt bo t where
>   Attr :: String -> Base t -> (t -> BindA sess stmt bo) -> Attr stmt bo t

A table schema is a list of attributes. We can reuse our |TList| datatype. The
|db| type variable is similar to |phi| in the previous sections: it is a family
with all the tables in our database.

> type Schema stmt bo db = HList2 (Attr stmt bo) db

%include CoreExample.lhs

A table is simply a schema with a name

> type Table stmt bo db = (String, Schema stmt bo db)
