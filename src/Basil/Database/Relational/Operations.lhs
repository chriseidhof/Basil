%if False

> {-# LANGUAGE PackageImports #-}

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Database.Relational.Schema
> import Basil.Data.TList
> import Database.HDBC
> import Database.HDBC.Sqlite3 (Connection)

> type DB = IO

%endif

We can now define some operations on our tables.
The implementations are straightforward and can be found in the library accompanying this thesis. 
All these operations are internally implemented using the |SQL| query language.
In our current library, they only work with the \emph{sqlite3}\footnote{\url{http://sqlite.org/}} database, we plan to support more database systems in the future.


> find'   :: Connection
>         -> Table t row
>         -> Int
>         -> IO (Maybe (HList row))
> find' conn (Table nm keys) x = do
>  let query = findSql x nm keys
>  print query
>  r <- quickQuery' conn query []
>  case r of
>   []    -> return Nothing
>   (x:_) -> return $ Just (parseRow keys x)

> create' :: Connection
>         -> Table t row
>         -> HList row
>         -> IO Int
> create' conn (Table nm keys) row = do
>   let query    = createSql nm keys
>       bindVals = tableSqlValues (zipHlistWithHList2 row keys)
>   print query
>   quickQuery' conn query bindVals

>   [[rowId]] <- quickQuery' conn "SELECT last_insert_rowid()" []
>     
>   return (fromSql rowId) -- TODO!

> createSql :: String
>           -> HList2 (Attr env) x
>           -> String
>
> createSql  nm keys = unwords
>  [ "INSERT INTO "
>  , nm
>  , parens (commaList $ tableSqlFields keys)
>  , "VALUES"
>  , parens (commaList $ tableSqlPlaceholders keys)
>  ]

> findSql :: Int -> String -> HList2 (Attr env) x -> String
> findSql x nm keys  = unwords 
>   [ "SELECT " 
>   , commaList (tableSqlFields keys)
>   , "FROM"
>   , nm
>   , "WHERE id ="
>   , show x
>   ]

> -- read    = undefined
> -- update  = undefined
> -- delete  = undefined
