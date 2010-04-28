%if False

> {-# LANGUAGE TypeOperators, FlexibleContexts #-}
>
> module Basil.Database.Relational.CoreExample where
>
> import Basil.Data.TList
> import Basil.Database.Relational.Core

%endif

We can now model an example schema for our database.
Later, we will see to do this automatically, but it is instructive to do this by hand.
We have four tables in our schema: one for each entity and a join table that links |Person| rows to |Compiler| rows.
The relationship between |Release| and |Compiler| is encoded by a foreign key in the |ReleaseRow| table:

> type CompilerDB = CompilerRow :*: PersonRow :*: ReleaseRow :*: Contributors :*: Nil
>
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

Constructing a row for the table is as easy as creating a value of type |HList
CompilerRow|. Note that the type enforces that the row matches exactly the
schema |CompilerRow|:

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

To construct a |Release| value that refers to the |Compiler| with id |42|, we define the following value:

> exampleRelease :: HList ReleaseRow
> exampleRelease = 612 .*. "11 Oct 2009" .*. "" .*. ForeignKey 42 .*. Nil

We have now seen how to construct a relational database schema with simple attributes and foreign keys, and how to build database schemas and rows.
By encoding the type of the schema at the type-level, we guarantee that all rows are well-formed with respect to the schema.
