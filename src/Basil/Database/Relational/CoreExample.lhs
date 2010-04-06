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

We can now model a schema for our database. We have four tables: one for each entity and a join table that links |Person| rows to |Compiler| rows.

> type CompilerDB = CompilerRow :*: PersonRow :*: ReleaseRow :*: Contributors :*: Nil

The schemas for each table are expressed on the type-level by type-level lists:

> type CompilerRow    = String :*: String :*: Nil
> type PersonRow      = String :*: String :*: String :*: Nil
> type ReleaseRow     = Int :*: String :*: String :*: Foreign CompilerRow :*: Nil
> type Contributors   = Foreign CompilerRow :*: Foreign PersonRow :*: Nil

As an example, we can construct a |Schema| and a |Table| for the |Compiler| table:

> compilerSchema :: Schema CompilerDB CompilerRow
> compilerSchema   =     Attr "name"      String
>                  .**.  Attr "homepage"  String
>                  .**.  Nil2

> compilerTable :: Table CompilerDB CompilerRow
> compilerTable = Table "compilers" compilerSchema

Constructing a row for the table is as easy as creating a value of type |HList CompilerRow|:

> exampleCompiler :: HList CompilerRow
> exampleCompiler = "GHC" .*. "http://haskell.org/ghc" .*. Nil

Constructing the table for |Release| entities involves the use of a foreign key:

> releaseTable :: Table CompilerDB ReleaseRow
> releaseTable = Table "releases" releaseSchema
>
> releaseSchema :: Schema CompilerDB ReleaseRow
> releaseSchema   =     Attr    "version"      Int
>                 .**.  Attr    "date"         String
>                 .**.  Attr    "notes"        String
>                 .**.  Foreign "compiler_id"  Zero
>                 .**.  Nil2

To construct a |Release| value that refers to the |Compiler| with the primary key |42|, we define the following value:

> exampleRelease :: HList ReleaseRow
> exampleRelease = 612 .*. "11 Oct 2009" .*. "" .*. ForeignKey 42 .*. Nil
