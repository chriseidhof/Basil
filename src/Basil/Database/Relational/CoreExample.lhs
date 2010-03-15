%if False

> {-# LANGUAGE TypeOperators #-}
>
> module Basil.Database.Relational.CoreExample where
>
> import Basil.Data.TList
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Operations
> import Basil.Database.Relational.Schema
> import Basil.Database.Relational.Takusen

%endif

We can now model a schema for a table |users|:

> type UserRow = String :*: Int :*: String :*: Nil
>
> userSchema :: Schema UserRow
> userSchema   =   Attr "name"   bindP String 
>             .**. Attr "age"    bindP Int 
>             .**. Attr "email"  bindP String 
>             .**. Nil2
> userTable :: Table UserRow
> userTable = ("users", userSchema)

Constructing a row for the table is as easy as constructing a value of
|UserRow|:

> exampleUser :: HList UserRow
> exampleUser = "chris" .*. 24 .*. "chris@example.com" .*. Nil


> insertUser :: IO Int
> insertUser = create userTable exampleUser

> -- test = createTable userTable
