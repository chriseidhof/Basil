%if False

> module Basil.Database.Relational.CoreExample where
>
> import Basil.Data.TList
> import Basil.Database.Relational.Core

%endif

We can now model a schema for a table |users|:

> type UserRow = (String, (Int, (String, ())))
>
> userSchema :: Schema db UserRow
> userSchema   =   Attr "name"   String 
>              .*  Attr "age"    Int 
>              .*  Attr "email"  String 
>              .*  ()

Constructing a row for the table is as easy as constructing a value of
|UserRow|:

> exampleUser = "chris" .* 24 .* "chris@example.com"
