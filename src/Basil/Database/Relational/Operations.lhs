%if False

> {-# LANGUAGE PackageImports #-}

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Database.Relational.Schema
> import Basil.Database.Relational.Entities
> import Basil.Data.TList
> import Database.HDBC
> import Database.HDBC.Sqlite3 (Connection)

> type DB = IO

%endif

We can now define some operations on our tables.
The implementations are straightforward and can be found in the library accompanying this thesis. 
All these operations are internally implemented using the |SQL| query language.
In our current library, they only work with the \emph{sqlite3}\footnote{\url{http://sqlite.org/}} database, we plan to support more database systems in the future.

> debug :: Show a => a -> IO ()
> debug = print

> createTable' :: Connection -> TableT a -> IO ()
> createTable' conn (TableT t _) = do
>   let query = createTableSql t
>   putStrLn (query ++ ";")


> findAll :: Connection
>         -> Table t row
>         -> Maybe String
>         -> IO [(HList row)]
> findAll conn (Table nm keys) cond = do
>  let query = findSql Nothing cond nm keys
>  debug query
>  r <- quickQuery' conn query []
>  return $ map (parseRow keys) r


> find'   :: Connection
>         -> Table t row
>         -> Int
>         -> IO (Maybe (HList row))
> find' conn (Table nm keys) x = do
>  let query = findSql (Just x) Nothing nm keys
>  debug query
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
>   debug query
>   quickQuery' conn query bindVals

>   [[rowId]] <- quickQuery' conn "SELECT last_insert_rowid()" []
>     
>   return (fromSql rowId) -- TODO!

> update'   :: Connection
>         -> Table t row
>         -> Int
>         -> HList row
>         -> IO ()
> update' conn (Table nm keys) x row = do
>  let query    = updateSql x nm keys
>      bindVals = tableSqlValues (zipHlistWithHList2 row keys)
>  r <- quickQuery' conn query  bindVals
>  return ()

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

> findSql :: Maybe Int -> Maybe String -> String -> HList2 (Attr env) x -> String
> findSql x cond nm keys  = unwords $
>   [ "SELECT " 
>   , commaList (tableSqlFields keys)
>   , "FROM"
>   , nm
>   ] ++ whereClause x cond
>  where
>    whereClause (Just x) Nothing  = [ "WHERE id =" , int x ]
>    whereClause (Just x) (Just c) = [ "WHERE id =" , int x, "AND", c]
>    whereClause Nothing  (Just c) = [ "WHERE", c]
>    whereClause Nothing  Nothing  = []

> updateSql x nm keys = unwords
>  [ "UPDATE"
>  , nm
>  , "SET"
>  , (commaList $ zipWith assign (tableSqlFields keys)
>                                (tableSqlPlaceholders keys)
>    )
>  , "WHERE id ="
>   , show x
>  ]

> assign x y = x ++ " = " ++ y

> int x = "'" ++ show x ++ "'"

> -- read    = undefined
> -- update  = undefined
> -- delete  = undefined
