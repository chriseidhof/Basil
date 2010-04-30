{-# LANGUAGE FlexibleContexts, GADTs #-}

module Basil.Database.Relational.Utils where

import Basil.Database.Relational.Core
import Basil.Data.HList
import Data.List (intercalate)
import Data.Char (toLower)
import Basil.Database.Relational.HDBC
import Database.HDBC

tableSqlFields :: HList2 (Attr env) x -> [String]
tableSqlFields = foldHList ((:) . createAttribute) []
 where createAttribute :: Attr env a -> String
       createAttribute (Attr nm _ )   = nm
       createAttribute (Foreign nm _) = nm

tableSqlPlaceholders :: HList2 (Attr env) x -> [String]
tableSqlPlaceholders = foldHList ((:) . const createPlaceHolder) []
 where createPlaceHolder :: String
       createPlaceHolder = "?"

tableSqlValues :: HList2 (WithMeta (Attr env)) row -> [SqlValue]
tableSqlValues = foldHList ((:) . createValue) []
 where createValue :: WithMeta (Attr env) a -> SqlValue
       createValue (Combined a (Attr _ typ))  = toHDBC typ a
       createValue (Combined a (Foreign _ _)) = toSql (foreignKey a)

parens :: String -> String
parens x = "(" ++ x ++ ")"

commaList :: [String] -> String
commaList = intercalate ","

lower :: String -> String
lower = map toLower
