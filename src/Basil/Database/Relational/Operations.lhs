%if False

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Data.TList
> import Database.Sqlite.Enumerator

%endif

We can now define some operations on our tables. The implementations are straightforward and can be found in the library accomplishing this thesis.

> create  :: Table row -> HList row -> IO Int
> read    :: Table row -> Int -> IO (Maybe (HList row))
> update  :: Table row -> Int -> HList row -> IO ()
> delete  :: Table row -> Int -> IO ()

%if False

> create (nm, keys) row = do
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
>  -- , parens (commaList $ tableSqlValues (zipHlistWithHList2 row keys))
>  ]

> read    = undefined
> update  = undefined
> delete  = undefined

%endif
