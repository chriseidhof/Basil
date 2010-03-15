This module implements the code to create database schemas.

%if False

> {-# LANGUAGE Rank2Types #-}
> 
> module Basil.Database.Relational.Schema where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Data.TList
> import Data.List (intercalate)

%endif

> createTable :: Table stmt bo row -> IO ()
> createTable (name, fields) = undefined

> createTableSql :: Table stmt bo row -> String
> createTableSql (name, fields) = unwords
>    [ "CREATE TABLE"
>    , name
>    , "("
>    , intercalate ", " (tableSqlFields fields)
>    , ")"
>    ]
