%if False

> {-# LANGUAGE PackageImports #-}

> module Basil.Database.Relational.Operations where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Database.Relational.Schema
> import Basil.Database.Relational.Entities
> import Basil.Data.HList
> import Database.HDBC
> import Database.HDBC.Sqlite3 (Connection)

> type DB = IO

> debug :: Show a => a -> IO ()
> debug = print

%endif

Now we define some operations on tables: for finding, creating, updating and deleting rows.
The implementations are straightforward and can be found in the library accompanying this thesis. 
All these operations are internally implemented using the untyped |SQL| query language. By providing a typed interface, it is guaranteed that no invalid |SQL| queries are constructed.

Our current library only supports the \emph{sqlite3}\cite{sqlite} database, but we plan to support more database systems in the future.

The |find'| function takes a connection, the description of a |Table| and the
integer that corresponds to the |id| field. The result of |find'| is a row
which has the type described by the |Table| value.

> find'   ::  Connection
>         ->  Table t row
>         ->  Int
>         ->  IO (Maybe (HList row))

The |findAll'| function is similar to find, except that it also takes a
condition. The condition is currently of type |Maybe String|, however, in a
later release we build a more type-safe variant. The result of |findAll'|
is a list of row values with their corresponding |id|s.

> findAll'  ::  Connection
>           ->  Table t row
>           ->  Maybe String
>           ->  IO [(Int, HList row)]

To create a new value the |create'| function is used. It takes an |HList row|
and returns the |id| value of the newly created row.

> create'  ::  Connection
>          ->  Table t row
>          ->  HList row
>          ->  IO Int

The function |update'| updates a |row| in a table, given the index of the row
and the new value.

> update'  ::  Connection
>          ->  Table t row
>          ->  Int
>          ->  HList row
>          ->  IO ()

Finally, |createTable'| creates a |Table|. This function is
different from the other functions: it does not manipulate data, but it creates
the SQL schema for a table.
When creating a new application, this function can be used to generate the database schema:

> createTable' :: Connection -> TableT a -> IO ()

At this point, we have created a way to model relational database schemas and have described the interface for creating and modifying data in the database. 
The next step is to use the encoding of the ER model to generate these schemas automatically, and provide an interface to the database that guarantees the constraints encoded in the ER model.

%if False

> createTable' _ (TableT t _) = do
>   let query = createTableSql t
>   putStrLn (query ++ ";")


> findAll' conn (Table nm keys) cond = do
>  let query = findAllSql cond nm keys
>  debug query
>  r <- quickQuery' conn query []
>  return $ map (parseRow' keys) r


> find' conn (Table nm keys) i = do
>  let query = findSql (Just i) Nothing nm keys
>  debug query
>  r <- quickQuery' conn query []
>  case r of
>   []    -> return Nothing
>   (x:_) -> return $ Just (parseRow keys x)

> create' conn (Table nm keys) row = do
>   let query    = createSql nm keys
>       bindVals = tableSqlValues (zipHlistWithHList2 row keys)
>   debug query
>   quickQuery' conn query bindVals

>   [[rowId]] <- quickQuery' conn "SELECT last_insert_rowid()" []
>     
>   return (fromSql rowId) -- TODO!

> update' conn (Table nm keys) x row = do
>  let query    = updateSql x nm keys
>      bindVals = tableSqlValues (zipHlistWithHList2 row keys)
>  quickQuery' conn query  bindVals
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
> findSql i cond nm keys  = unwords $
>   [ "SELECT " 
>   , commaList (tableSqlFields keys)
>   , "FROM"
>   , nm
>   ] ++ whereClause i cond
>  where
>    whereClause (Just x) Nothing  = [ "WHERE id =" , int x ]
>    whereClause (Just x) (Just c) = [ "WHERE id =" , int x, "AND", c]
>    whereClause Nothing  (Just c) = [ "WHERE", c]
>    whereClause Nothing  Nothing  = []

> findAllSql :: Maybe String -> String -> HList2 (Attr env) x -> String
> findAllSql cond nm keys  = unwords $
>   [ "SELECT " 
>   , commaList ("id" : tableSqlFields keys)
>   , "FROM"
>   , nm
>   ] ++ whereClause cond
>  where
>    whereClause (Just c) = [ "WHERE", c]
>    whereClause Nothing  = []

> updateSql :: Int -> String -> HList2 (Attr env) x -> String
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

> assign :: String -> String -> String
> assign x y = x ++ " = " ++ y

> int :: Int -> String
> int x = "'" ++ show x ++ "'"

%endif
