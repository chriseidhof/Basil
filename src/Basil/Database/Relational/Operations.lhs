%if False

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Data.TList
> import Database.Sqlite.Enumerator

> type DB = IO

%endif

We can now define some operations on our tables.
The implementations are straightforward and can be found in the library accompanying this thesis. 
All these operations are internally implemented using the |SQL| query language.
In our current library, they only work with the \emph{sqlite3}\footnote{\url{http://sqlite.org/}} database, we plan to support more database systems in the future.


> create  :: Table env row -> HList row -> DB Int
> read    :: Table env row -> Int -> DB (Maybe (HList row))
> update  :: Table env row -> Int -> HList row -> DB ()
> delete  :: Table env row -> Int -> DB ()

%if False

> create (Table nm keys) row = do
>   let query    = createSql (nm, keys)
>       bindVals = tableSqlValues (zipHlistWithHList2 row keys)
>   withSession (connect "test.sqlite3") ( execDML (cmdbind query bindVals))
>   putStrLn query
>     
>   return 1
>
> createSql (nm, keys) = unwords
>  [ "INSERT INTO "
>  , nm
>  , parens (commaList $ tableSqlFields keys)
>  , "VALUES"
>  , parens (commaList $ tableSqlPlaceholders keys)
>  ]

> read    = undefined
> update  = undefined
> delete  = undefined

%endif
