%if False

> {-# LANGUAGE TypeOperators, FlexibleContexts #-}
>
> module Basil.Database.Relational.CoreExample where
>
> import Basil.Data.TList
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Operations
> import Basil.Database.Relational.Schema
> import Basil.Database.Relational.Takusen
> import Database.Enumerator
> import Database.Sqlite.Enumerator

%endif

We can now model a schema for a table |users|:

> type UserRow = String :*: Int :*: String :*: Nil
>
> userSchema :: (  DBBind Int    Session stmt bo
>               ,  DBBind String Session stmt bo
>               )
>            => Schema stmt bo UserRow
> userSchema   =   Attr "name"   String bindP
>             .**. Attr "age"    Int    bindP
>             .**. Attr "email"  String bindP
>             .**. Nil2

> userTable :: (  DBBind Int    Session stmt bo
>               ,  DBBind String Session stmt bo
>               )
>            => Table stmt bo UserRow
> userTable = ("users", userSchema)

Constructing a row for the table is as easy as constructing a value of
|UserRow|:

> exampleUser :: HList UserRow
> exampleUser = "chris" .*. 24 .*. "chris@example.com" .*. Nil


> insertUser :: IO Int
> insertUser = create userTable exampleUser

> -- test = createTable userTable
