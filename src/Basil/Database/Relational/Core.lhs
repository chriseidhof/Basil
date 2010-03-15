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

> data Attr t where
>   Attr :: String -> (t -> BindA sess stmt bo) -> Base t -> Attr t

A table schema is a list of attributes. We can reuse our |TList| datatype. The
|db| type variable is similar to |phi| in the previous sections: it is a family
with all the tables in our database.

> type Schema db = HList2 Attr db

%include CoreExample.lhs

A table is simply a schema with a name

> type Table db = (String, Schema db)
