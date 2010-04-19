This module implements the code to create database schemas.

%if False

> {-# LANGUAGE Rank2Types, GADTs #-}
> 
> module Basil.Database.Relational.Schema where
>
> import Basil.Database.Relational.Core
> import Basil.Database.Relational.Utils
> import Basil.Database.Relational.HDBC
> import Data.List (intercalate)
> import Database.HDBC
> import Basil.Data.TList

%endif

> createTableSql :: Table env row -> String
> createTableSql (Table name fields) = unwords
>    [ "CREATE TABLE"
>    , name
>    , "("
>    , intercalate ", " ("id INTEGER PRIMARY KEY AUTOINCREMENT" : tableSqlFields fields)
>    , ")"
>    ]

> parseRow :: Schema env row -> [SqlValue] -> HList row
> parseRow Nil2 [] = Nil
> parseRow (Cons2 x xs) (y:ys) = fromHDBC x y .*. parseRow xs ys
> parseRow _ _  = error "parseRow"
